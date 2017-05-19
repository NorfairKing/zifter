{-# LANGUAGE DeriveGeneric #-}

module Zifter.Zift.Types
    ( ZiftOutputMessage(..)
    , ZiftOutput(..)
    , ZiftContext(..)
    , LR(..)
    , RecursionPath(..)
    , Zift
    , ZiftResult(..)
    , getContext
    , addZiftOutput
    , runZift
    , BookkeeperState(..)
    , OutputRecord(..)
    , OutputBuffer(..)
    , singletonBuffer
    , bufferOf
    , bufferToList
    , initialBookkeeperState
    , advanceBookkeeperState
    , shouldFlush
    ) where

import Prelude

import Control.Concurrent.Async (waitEither, wait, cancel, async)
import Control.Concurrent.STM
       (TChan, writeTChan, atomically, TMVar, readTChan, orElse, putTMVar,
        newEmptyTMVar, takeTMVar, tryReadTChan)
import Control.Exception (SomeException, displayException, catch)
import Control.Monad
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Path
import Path.IO
import System.Console.ANSI
import System.Exit
import System.IO

import Zifter.OptParse.Types

data ZiftOutputMessage
    = CompletionMessage RecursionPath
    | ZiftOutputMessage RecursionPath
                        ZiftOutput
    deriving (Show, Eq, Generic)

data ZiftOutput = ZiftOutput
    { outputColors :: [SGR]
    , outputMessage :: String
    } deriving (Show, Eq, Ord, Generic)

data ZiftContext = ZiftContext
    { rootdir :: Path Abs Dir
    , settings :: Settings
    , printChan :: TChan ZiftOutputMessage -- The LR is in reverse order.
    , recursionList :: RecursionPath -- In reverse order
    } deriving (Generic)

data LR
    = L
    | R
    deriving (Show, Eq, Ord, Generic)

newtype RecursionPath =
    RecursionPath [LR] -- In reverse order
    deriving (Show, Eq, Ord, Generic)

instance Monoid RecursionPath where
    mempty = RecursionPath []
    mappend (RecursionPath r1) (RecursionPath r2) = RecursionPath (r2 ++ r1) -- Reverse on purpose

recurse :: LR -> RecursionPath -> RecursionPath
recurse e (RecursionPath ls) = RecursionPath (e : ls)

instance Validity ZiftContext where
    isValid = isValid . rootdir

newtype Zift a = Zift
    { zift :: ZiftContext -> IO (ZiftResult a)
    } deriving (Generic)

instance Monoid a =>
         Monoid (Zift a) where
    mempty = Zift $ \_ -> pure mempty
    mappend z1 z2 = mappend <$> z1 <*> z2

instance Functor Zift where
    fmap f (Zift iof) =
        Zift $ \rd -> do
            r <- iof rd
            let rs = fmap f r
            sendMessage rd (CompletionMessage $ recursionList rd)
            pure rs

-- | 'Zift' actions can be sequenced.
--
-- The implementation automatically parallelises the arguments of the
-- @(<*>)@ function. If any of the actions fails, the other is cancelled
-- and the result fails.
instance Applicative Zift where
    pure a = Zift $ \_ -> pure $ pure a
    (Zift faf) <*> (Zift af) =
        Zift $ \zc -> do
            let zc1 = zc {recursionList = recurse L $ recursionList zc}
                zc2 = zc {recursionList = recurse R $ recursionList zc}
            afaf <- async $ faf zc1
            aaf <- async $ af zc2
            efaa <- waitEither afaf aaf
            r <-
                case efaa of
                    Left far ->
                        case far of
                            ZiftFailed s -> do
                                cancel aaf
                                pure $ ZiftFailed s
                            _ -> do
                                t2 <- wait aaf
                                pure $ far <*> t2
                    Right ar ->
                        case ar of
                            ZiftFailed s -> do
                                cancel afaf
                                pure $ ZiftFailed s
                            _ -> do
                                t1 <- wait afaf
                                pure $ t1 <*> ar
            sendMessage zc (CompletionMessage $ recursionList zc)
            pure r

-- | 'Zift' actions can be composed.
instance Monad Zift where
    (Zift fa) >>= mb =
        Zift $ \rd -> do
            ra <- fa (rd {recursionList = recurse L $ recursionList rd})
            r <-
                case ra of
                    ZiftSuccess a ->
                        case mb a of
                            Zift pb ->
                                pb
                                    (rd
                                     { recursionList =
                                           recurse R $ recursionList rd
                                     })
                    ZiftFailed e -> pure (ZiftFailed e)
            sendMessage rd (CompletionMessage $ recursionList rd)
            pure r
    fail = Fail.fail

-- | A 'Zift' action can fail.
--
-- To make a Zift action fail, you can use the @fail :: String -> Zift a@
-- function.
--
-- The implementation uses the given string as the message that is shown at
-- the very end of the run.
instance MonadFail Zift where
    fail s = Zift $ \_ -> pure (ZiftFailed [s])

-- | Any IO action can be part of a 'Zift' action.
--
-- This is the most important instance for the end user.
--
-- > liftIO :: IO a -> Zift a
-- allows embedding arbitrary IO actions inside a 'Zift' action.
--
-- The implementation also ensures that exceptions are caught.
instance MonadIO Zift where
    liftIO act =
        Zift $ \rd -> do
            res <- (act >>= (pure . ZiftSuccess)) `catch` handler
            sendMessage rd (CompletionMessage $ recursionList rd)
            pure res
      where
        handler :: SomeException -> IO (ZiftResult a)
        handler ex = pure (ZiftFailed [displayException ex])

instance MonadThrow Zift where
    throwM e = Zift $ \_ -> throwM e

data ZiftResult a
    = ZiftSuccess a
    | ZiftFailed [String]
    deriving (Show, Eq, Generic)

instance Validity a =>
         Validity (ZiftResult a) where
    isValid (ZiftSuccess a) = isValid a
    isValid _ = True

instance Monoid a =>
         Monoid (ZiftResult a) where
    mempty = ZiftSuccess mempty
    mappend z1 z2 = mappend <$> z1 <*> z2

instance Functor ZiftResult where
    fmap f (ZiftSuccess a) = ZiftSuccess $ f a
    fmap _ (ZiftFailed s) = ZiftFailed s

instance Applicative ZiftResult where
    pure = ZiftSuccess
    (ZiftSuccess f) <*> (ZiftSuccess a) = ZiftSuccess $ f a
    (ZiftFailed e) <*> (ZiftSuccess _) = ZiftFailed e
    (ZiftSuccess _) <*> (ZiftFailed e) = ZiftFailed e
    (ZiftFailed e1) <*> (ZiftFailed e2) = ZiftFailed $ e1 ++ e2

instance Monad ZiftResult where
    (ZiftSuccess a) >>= fb = fb a
    (ZiftFailed e) >>= _ = ZiftFailed e

instance MonadFail ZiftResult where
    fail s = ZiftFailed [s]

getContext :: Zift ZiftContext
getContext = Zift $ \zc -> pure $ ZiftSuccess zc

addZiftOutput :: ZiftOutput -> Zift ()
addZiftOutput zo =
    Zift $ \zc -> do
        sendMessage zc $ ZiftOutputMessage (recursionList zc) zo
        pure mempty

sendMessage :: ZiftContext -> ZiftOutputMessage -> IO ()
sendMessage zc zom = atomically $ writeTChan (printChan zc) zom

runZift :: ZiftContext -> Zift () -> IO ExitCode
runZift ctx zfunc = do
    fmvar <- atomically newEmptyTMVar
    printerAsync <- async (runZiftBookkeeper fmvar ctx)
    runnerAsync <- async (runZiftRunner fmvar ctx zfunc)
    result <- wait runnerAsync
    wait printerAsync
    pure result

runZiftRunner :: TMVar () -> ZiftContext -> Zift () -> IO ExitCode
runZiftRunner fmvar ctx zfunc = do
    let pchan = printChan ctx
    withSystemTempDir "zifter" $ \d ->
        withCurrentDir d $ do
            r <- zift zfunc ctx
            result <-
                case r of
                    ZiftFailed errs -> do
                        atomically $
                            forM_ errs $ \err ->
                                writeTChan pchan $
                                ZiftOutputMessage mempty $
                                ZiftOutput [SetColor Foreground Dull Red] err
                        pure $ ExitFailure 1
                    ZiftSuccess () -> pure ExitSuccess
            atomically $ putTMVar fmvar ()
            pure result

runZiftBookkeeper :: TMVar () -> ZiftContext -> IO ()
runZiftBookkeeper fmvar ctx = printer
  where
    pchan = printChan ctx
    sets = settings ctx
    outputOne :: ZiftOutput -> IO ()
    outputOne (ZiftOutput commands str) = do
        let color = setsOutputColor sets
        when color $ setSGR commands
        putStr str
        when color $ setSGR [Reset]
        putStr "\n" -- Because otherwise it doesn't work?
        hFlush stdout
    -- outputAll = do
    --     mout <- atomically $ tryReadTChan pchan
    --     case mout of
    --         Nothing -> pure ()
    --         Just output -> do
    --             outputOne output
    --             outputAll
    printer = go initialBookkeeperState
      where
        go :: BookkeeperState -> IO ()
        go bs = do
            mdone <-
                atomically $
                (Left <$> takeTMVar fmvar) `orElse` (Right <$> readTChan pchan)
            case mdone of
                Left () -> pure () -- outputAll
                Right output -> do
                    case advanceBookkeeperState bs output of
                        Nothing -> pure ()
                        Just (bs', buf) -> do
                            mapM_ outputOne $ bufferToList buf
                            -- print buf
                            -- print bs'
                            go bs'

newtype OutputBuffer =
    OutputBuffer [ZiftOutput] -- In reverse order.
    deriving (Show, Eq, Ord, Generic)

instance Monoid OutputBuffer where
    mempty = OutputBuffer []
    mappend (OutputBuffer o1) (OutputBuffer o2) = OutputBuffer $ o2 ++ o1

singletonBuffer :: ZiftOutput -> OutputBuffer
singletonBuffer = OutputBuffer . (: [])

bufferOf :: [ZiftOutput] -> OutputBuffer
bufferOf = OutputBuffer . reverse

bufferToList :: OutputBuffer -> [ZiftOutput]
bufferToList (OutputBuffer zos) = reverse zos

data OutputRecord
    = Complete OutputBuffer
    | Incomplete OutputBuffer
    | Flushed
    deriving (Show, Eq, Ord, Generic)

data BookkeeperState = BookkeeperState
    { bufferedMessages :: Map RecursionPath OutputRecord
    } deriving (Show, Eq, Generic)

initialBookkeeperState :: BookkeeperState
initialBookkeeperState = BookkeeperState {bufferedMessages = M.empty}

advanceBookkeeperState
    :: BookkeeperState
    -> ZiftOutputMessage
    -> Maybe (BookkeeperState, OutputBuffer) -- Nothing means done.
advanceBookkeeperState (BookkeeperState bm) om =
    let continueWith :: Map RecursionPath OutputRecord
                     -> Maybe (BookkeeperState, OutputBuffer)
        continueWith bm' = Just (BookkeeperState bm', mempty)
    in case om of
           ZiftOutputMessage rp zo ->
               case M.lookup rp bm of
                   Nothing -- No record
                    ->
                       continueWith $
                       M.insert rp (Incomplete $ singletonBuffer zo) bm
                   Just or -- There is a record already
                    ->
                       case or of
                           Complete b -- The record states that this path is already complete.
                                    -- This should never happen, but if it does' we will just add the message to the buffer anyway, because it has not been flushed yet.
                            ->
                               continueWith $
                               M.insert
                                   rp
                                   (Complete $ b `mappend` singletonBuffer zo)
                                   bm
                           Incomplete ob -- The record states that this path is incomplete.
                                         -- We just add to the buffer.
                            ->
                               continueWith $
                               M.insert
                                   rp
                                   (Incomplete $ ob `mappend` singletonBuffer zo)
                                   bm
                           Flushed -- The record states that this path is already flushed.
                                      -- This should never happen, but it is not a problem.
                                      -- In this case we just discard the message and leave the record 'Flushed'.
                            -> continueWith bm
           CompletionMessage rp ->
               case M.lookup rp bm of
                   Nothing -- No record yet. This message completes the path.
                    ->
                       if shouldFlush (BookkeeperState bm) rp
                           then continueWith $ M.insert rp Flushed bm
                           else continueWith $ M.insert rp (Complete mempty) bm
                   Just r ->
                       case r of
                           Incomplete ob -- The record states that this path is incomplete, then this message completes it.
                            ->
                               Just $
                               if shouldFlush (BookkeeperState bm) rp
                                   then ( BookkeeperState $
                                          M.insert rp Flushed bm
                                        , ob)
                                   else ( BookkeeperState $
                                          M.insert rp (Complete ob) bm
                                        , mempty)
                           Complete ob -- The record is states that this path is already complete.
                             -- This should never happen, but if it does, we will just leave it completed.
                            -> continueWith bm
                           Flushed -- The record states that this path is already flushed.
                                      -- This should never happen, but it is not a problem.
                                      -- We just leave the record 'Flushed'.
                            -> continueWith bm

-- Returns true if the buffer should be flushed upon receiving a completion message.
-- Also returns true for any path that should already have been flushed.
shouldFlush :: BookkeeperState -> RecursionPath -> Bool
shouldFlush bs@(BookkeeperState bm) (RecursionPath rp) =
    case rp of
        [] -> True
        (L:rest) -> shouldFlush bs $ RecursionPath rest
        (R:rest) ->
            case M.lookup (RecursionPath (L : rest)) bm of
                Nothing -> False
                Just or ->
                    case or of
                        Flushed -> True
                        _ -> False
