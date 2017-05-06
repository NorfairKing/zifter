{-# LANGUAGE DeriveGeneric #-}

module Zifter.Zift.Types
    ( ZiftOutputMessage(..)
    , ZiftOutput(..)
    , ZiftContext(..)
    , LR
    , Zift
    , ZiftResult(..)
    , getContext
    , addZiftOutput
    ) where

import Prelude

import Control.Concurrent.Async (waitEither, wait, cancel, async)
import Control.Concurrent.STM (TChan, writeTChan, atomically)
import Control.Exception (SomeException, displayException, catch)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Path
import System.Console.ANSI (SGR)

import Zifter.OptParse.Types

data ZiftOutputMessage = ZiftOutputMessage
    { ziftOutputMessageOriginator :: [LR]
    , ziftOutputMessage :: ZiftOutput
    } deriving (Show, Eq, Generic)

data ZiftOutput = ZiftOutput
    { outputColors :: [SGR]
    , outputMessage :: String
    } deriving (Show, Eq, Generic)

data ZiftContext = ZiftContext
    { rootdir :: Path Abs Dir
    , settings :: Settings
    , printChan :: TChan ZiftOutputMessage -- The LR is in reverse order.
    , recursionList :: [LR] -- In reverse order
    } deriving (Generic)

data LR
    = L
    | R
    deriving (Show, Eq, Generic)

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
            pure $ fmap f r

-- | 'Zift' actions can be sequenced.
--
-- The implementation automatically parallelises the arguments of the
-- @(<*>)@ function. If any of the actions fails, the other is cancelled
-- and the result fails.
instance Applicative Zift where
    pure a = Zift $ \_ -> pure $ pure a
    (Zift faf) <*> (Zift af) =
        Zift $ \zc -> do
            let zc1 = zc {recursionList = L : recursionList zc}
                zc2 = zc {recursionList = R : recursionList zc}
            afaf <- async $ faf zc1
            aaf <- async $ af zc2
            efaa <- waitEither afaf aaf
            let complete fa a = pure $ fa <*> a
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

-- | 'Zift' actions can be composed.
instance Monad Zift where
    (Zift fa) >>= mb =
        Zift $ \rd -> do
            let newlist = (L : recursionList rd)
            ra <- fa (rd {recursionList = newlist})
            case ra of
                ZiftSuccess a ->
                    case mb a of
                        Zift pb ->
                            pb (rd {recursionList = R : recursionList rd})
                ZiftFailed e -> pure (ZiftFailed e)
    fail = Fail.fail

-- | A 'Zift' action can fail.
--
-- To make a Zift action fail, you can use the @fail :: String -> Zift a@
-- function.
--
-- The implementation uses the given string as the message that is shown at
-- the very end of the run.
instance MonadFail Zift where
    fail s = Zift $ \_ -> pure (ZiftFailed s)

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
        Zift $ \_ -> (act >>= (\r -> pure $ ZiftSuccess r)) `catch` handler
      where
        handler :: SomeException -> IO (ZiftResult a)
        handler ex = pure (ZiftFailed $ displayException ex)

instance MonadThrow Zift where
    throwM e = Zift $ \_ -> throwM e

data ZiftResult a
    = ZiftSuccess a
    | ZiftFailed String
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
    (ZiftFailed e1) <*> (ZiftFailed e2) = ZiftFailed $ unwords [e1, e2]

instance Monad ZiftResult where
    (ZiftSuccess a) >>= fb = fb a
    (ZiftFailed e) >>= _ = ZiftFailed e

instance MonadFail ZiftResult where
    fail = ZiftFailed

getContext :: Zift ZiftContext
getContext = Zift $ \zc -> pure $ ZiftSuccess zc

addZiftOutput :: ZiftOutput -> Zift ()
addZiftOutput zo =
    Zift $ \zc -> do
        atomically $
            writeTChan (printChan zc) $
            ZiftOutputMessage
            { ziftOutputMessage = zo
            , ziftOutputMessageOriginator = recursionList zc
            }
        pure mempty

runZift :: ZiftContext -> Zift () -> IO ExitCode
runZift ctx zfunc = do
    let pchan = printChan ctx
        sets = settings ctx
    fmvar <- atomically newEmptyTMVar
    printerAsync <- async (runZiftBookkeeper pchan)
    runnerAsync <- async (runZiftRunner ctx)
    result <- wait runnerAsync
    wait printerAsync
    pure result

runZiftRunner :: TMVar () -> ZiftContext -> IO ()
runZiftRunner fmvar ctx = do
    let pchan = printChan ctx
        sets = settings ctx
    withSystemTempDir "zifter" $ \d ->
        withCurrentDir d $ do
            (r, zs) <- zift zfunc ctx mempty
            result <-
                case r of
                    ZiftFailed err -> do
                        atomically $
                            writeTChan pchan $
                            ZiftOutputMessage [] $
                            ZiftOutput [SetColor Foreground Dull Red] err
                        pure $ ExitFailure 1
                    ZiftSuccess () -> pure ExitSuccess
            atomically $ putTMVar fmvar ()
            pure result

runZiftBookkeeper :: TMVar () -> ZiftContext -> IO ()
runZiftBookkeeper fmvar ctx = do
    let pchan = printChan ctx
        sets = settings ctx
    let outputOne :: ZiftOutput -> IO ()
        outputOne (ZiftOutput commands str) = do
            let color = setsOutputColor sets
            when color $ setSGR commands
            putStr str
            when color $ setSGR [Reset]
            putStr "\n" -- Because otherwise it doesn't work?
            hFlush stdout
    let outputAll = do
            mout <- atomically $ tryReadTChan pchan
            case mout of
                Nothing -> pure ()
                Just output -> do
                    outputOne $ ziftOutputMessage output
                    outputAll
    let printer = do
            mdone <-
                atomically $
                (Left <$> takeTMVar fmvar) `orElse` (Right <$> readTChan pchan)
            case mdone of
                Left () -> outputAll
                Right output -> do
                    outputOne $ ziftOutputMessage output
                    printer
    printer
