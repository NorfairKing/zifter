{-# LANGUAGE DeriveGeneric #-}

module Zifter.Zift.Types where

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

data ZiftOutput = ZiftOutput
    { outputColors :: [SGR]
    , outputMessage :: String
    } deriving (Show, Eq, Generic)

data ZiftContext = ZiftContext
    { rootdir :: Path Abs Dir
    , settings :: Settings
    , printChan :: TChan ZiftOutput
    , recursionList :: [LR] -- In reverse order
    } deriving (Generic)

data LR
    = L
    | R
    deriving (Show, Eq, Generic)

instance Validity ZiftContext where
    isValid = isValid . rootdir

newtype ZiftState = ZiftState
    { bufferedOutput :: [ZiftOutput] -- In reverse order
    } deriving (Show, Eq, Generic)

instance Monoid ZiftState where
    mempty = ZiftState {bufferedOutput = []}
    mappend zs1 zs2 =
        ZiftState
        {bufferedOutput = bufferedOutput zs2 `mappend` bufferedOutput zs1}

newtype Zift a = Zift
    { zift :: ZiftContext -> ZiftState -> IO (ZiftResult a, ZiftState)
    } deriving (Generic)

instance Monoid a => Monoid (Zift a) where
    mempty = Zift $ \_ s -> pure (mempty, s)
    mappend z1 z2 = mappend <$> z1 <*> z2

instance Functor Zift where
    fmap f (Zift iof) =
        Zift $ \rd st -> do
            (r, st') <- iof rd st
            st'' <- tryFlushZiftBuffer rd st'
            pure (fmap f r, st'')

instance Applicative Zift where
    pure a = Zift $ \_ st -> pure (pure a, st)
    (Zift faf) <*> (Zift af) =
        Zift $ \zc st -> do
            let zc1 = zc {recursionList = L : recursionList zc}
                zc2 = zc {recursionList = R : recursionList zc}
            afaf <- async (faf zc1 mempty)
            aaf <- async (af zc2 mempty)
            efaa <- waitEither afaf aaf
            let complete (fa, zs1) (a, zs2) = do
                    let st' = st `mappend` zs1 `mappend` zs2
                    st'' <- tryFlushZiftBuffer zc st'
                    pure (fa <*> a, st'')
            case efaa of
                Left t1@(far, zs1) ->
                    case far of
                        ZiftFailed s -> do
                            cancel aaf
                            pure (ZiftFailed s, st `mappend` zs1)
                        _ -> do
                            t2 <- wait aaf
                            complete t1 t2
                Right t2@(ar, zs2) ->
                    case ar of
                        ZiftFailed s -> do
                            cancel afaf
                            pure (ZiftFailed s, st `mappend` zs2)
                        _ -> do
                            t1 <- wait afaf
                            complete t1 t2

instance Monad Zift where
    (Zift fa) >>= mb =
        Zift $ \rd st -> do
            (ra, st') <- fa rd st
            st'' <- tryFlushZiftBuffer rd st'
            case ra of
                ZiftSuccess a ->
                    case mb a of
                        Zift pb -> pb rd st''
                ZiftFailed e -> pure (ZiftFailed e, st'')
    fail = Fail.fail

instance MonadFail Zift where
    fail s = Zift $ \_ st -> pure (ZiftFailed s, st)

instance MonadIO Zift where
    liftIO act =
        Zift $ \_ st ->
            (act >>= (\r -> pure (ZiftSuccess r, st))) `catch` handler st
      where
        handler :: ZiftState -> SomeException -> IO (ZiftResult a, ZiftState)
        handler s ex = pure (ZiftFailed $ displayException ex, s)

instance MonadThrow Zift where
    throwM e = Zift $ \_ _ -> throwM e

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

-- | Internal: do not use yourself.
tryFlushZiftBuffer :: ZiftContext -> ZiftState -> IO ZiftState
tryFlushZiftBuffer ctx st =
    if null $ recursionList ctx
        then do
            let zos = reverse $ bufferedOutput st
                st' = st {bufferedOutput = []}
            atomically $ mapM_ (writeTChan $ printChan ctx) zos
            pure st'
        else pure st
