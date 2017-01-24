{-# LANGUAGE DeriveGeneric #-}

module Zifter.Zift.Types where

import Prelude hiding (fail)

import Control.Concurrent (MVar)
import Control.Concurrent.Async (async, wait)
import Control.Exception (SomeException, displayException, catch)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Validity
import GHC.Generics
import Path

import Zifter.OptParse.Types

data ZiftContext = ZiftContext
    { rootdir :: Path Abs Dir
    , settings :: Settings
    , printvar :: MVar ()
    } deriving (Eq, Generic)

instance Validity ZiftContext where
    isValid _ = True -- TODO check validity of the root dir.

newtype Zift a = Zift
    { zift :: ZiftContext -> IO (ZiftResult a)
    } deriving (Generic)

instance Monoid a =>
         Monoid (Zift a) where
    mempty = Zift $ \_ -> pure mempty
    mappend z1 z2 = Zift $ \rd -> mappend <$> zift z1 rd <*> zift z2 rd

instance Functor Zift where
    fmap f (Zift iof) =
        Zift $ \rd -> do
            r <- iof rd
            pure $ fmap f r

instance Applicative Zift where
    pure a = Zift $ \_ -> pure $ pure a
    (Zift faf) <*> (Zift af) =
        Zift $ \zc -> do
            faa <- async $ faf zc
            aa <- async $ af zc
            fa <- wait faa
            a <- wait aa
            pure $ fa <*> a

instance Monad Zift where
    (Zift fa) >>= mb =
        Zift $ \rd -> do
            ra <- fa rd
            case ra of
                ZiftSuccess a ->
                    case mb a of
                        Zift pb -> pb rd
                ZiftFailed e -> pure $ ZiftFailed e

instance MonadFail Zift where
    fail s = Zift $ \_ -> pure $ fail s

instance MonadIO Zift where
    liftIO act = Zift $ \_ -> (ZiftSuccess <$> act) `catch` handler
      where
        handler :: SomeException -> IO (ZiftResult a)
        handler ex = pure $ ZiftFailed $ displayException ex

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
