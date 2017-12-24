{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module Zifter.Zift.Types where

import Prelude

import Control.Concurrent.Async (async, cancel, wait, waitEither)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Exception (SomeException, catch, displayException)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Path
import System.Console.ANSI (SGR)

import Zifter.OptParse.Types

data ZiftToken
    = TokenOutput [LR]
                  ZiftOutput
    | TokenDone [LR]
    deriving (Show, Eq, Generic)

data ZiftOutput = ZiftOutput
    { outputColors :: [SGR]
    , outputMessage :: String
    } deriving (Show, Eq, Generic)

data ZiftContext = ZiftContext
    { rootdir :: Path Abs Dir
    , tmpdir :: Path Abs Dir
    , settings :: Settings
    , printChan :: TChan ZiftToken
    , recursionList :: [LR] -- In reverse order
    } deriving (Generic)

data LR
    = L
    | R
    deriving (Show, Eq, Generic)

instance Validity ZiftContext where
    isValid = isValid . rootdir
#if MIN_VERSION_validity(0,4,0)
    validate zc = rootdir zc <?!> "rootdir"
#endif
newtype ZiftState = ZiftState
    { bufferedOutput :: [ZiftOutput] -- In reverse order
    } deriving (Show, Eq, Generic)

instance Monoid ZiftState where
    mempty = ZiftState {bufferedOutput = []}
    mappend zs1 zs2 =
        ZiftState
        {bufferedOutput = bufferedOutput zs2 `mappend` bufferedOutput zs1}

newtype Zift a = Zift
    { zift :: ZiftContext -> IO (ZiftResult a)
    } deriving (Generic)

instance Monoid a => Monoid (Zift a) where
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
            let left = L : recursionList zc
                right = R : recursionList zc
                zc1 = zc {recursionList = left}
                zc2 = zc {recursionList = right}
            afaf <- async $ faf zc1
            aaf <- async $ af zc2
            efaa <- waitEither afaf aaf
            let complete fa a = pure $ fa <*> a
            case efaa of
                Left far -> do
                    finishSection left $ printChan zc
                    r <-
                        case far of
                            ZiftFailed s -> do
                                cancel aaf
                                pure $ ZiftFailed s
                            _ -> do
                                t2 <- wait aaf
                                complete far t2
                    finishSection right $ printChan zc
                    pure r
                Right ar -> do
                    finishSection right $ printChan zc
                    r <-
                        case ar of
                            ZiftFailed s -> do
                                cancel afaf
                                pure $ ZiftFailed s
                            _ -> do
                                t1 <- wait afaf
                                complete t1 ar
                    finishSection left $ printChan zc
                    pure r

-- | 'Zift' actions can be composed.
instance Monad Zift where
    (Zift fa) >>= mb =
        Zift $ \rd -> do
            let left = L : recursionList rd
            ra <- fa rd {recursionList = left}
            finishSection left $ printChan rd
            case ra of
                ZiftSuccess a ->
                    case mb a of
                        Zift pb -> do
                            let right = R : recursionList rd
                            res <- pb rd {recursionList = right}
                            finishSection right $ printChan rd
                            pure res
                ZiftFailed e -> pure $ ZiftFailed e
    fail = Fail.fail

finishSection :: [LR] -> TChan ZiftToken -> IO ()
finishSection ls pchan = atomically $ writeTChan pchan $ TokenDone ls

-- | A 'Zift' action can fail.
--
-- To make a Zift action fail, you can use the @fail :: String -> Zift a@
-- function.
--
-- The implementation uses the given string as the message that is shown at
-- the very end of the run.
instance MonadFail Zift where
    fail s = Zift $ \_ -> pure $ ZiftFailed s

-- | Any IO action can be part of a 'Zift' action.
--
-- This is the most important instance for the end user.
--
-- > liftIO :: IO a -> Zift a
-- allows embedding arbitrary IO actions inside a 'Zift' action.
--
-- The implementation also ensures that exceptions are caught.
instance MonadIO Zift where
    liftIO act = Zift $ \_ -> (ZiftSuccess <$> act) `catch` handler
      where
        handler :: SomeException -> IO (ZiftResult a)
        handler ex = pure (ZiftFailed $ displayException ex)

instance MonadThrow Zift where
    throwM e = Zift $ \_ -> throwM e

data ZiftResult a
    = ZiftSuccess a
    | ZiftFailed String
    deriving (Show, Eq, Generic)

instance Validity a => Validity (ZiftResult a) where
    isValid (ZiftSuccess a) = isValid a
    isValid _ = True

instance Monoid a => Monoid (ZiftResult a) where
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
-- -- | Internal: do not use yourself.
-- tryFlushZiftBuffer :: ZiftContext -> ZiftState -> IO ZiftState
-- tryFlushZiftBuffer ctx st =
--     if flushable $ recursionList ctx
--         then do
--             let zos = reverse $ bufferedOutput st
--                 st' = st {bufferedOutput = []}
--             atomically $ mapM_ (writeTChan $ printChan ctx) zos
--             pure st'
--         else pure st
--
-- -- The buffer is flushable when it's guaranteed to be the first in the in-order
-- -- of the evaluation tree.
-- flushable :: [LR] -> Bool
-- flushable = all (== L)
