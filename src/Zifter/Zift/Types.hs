{-# LANGUAGE DeriveGeneric #-}

module Zifter.Zift.Types where

import Introduction

import Control.Monad.Fail

data Zift a = Zift
    { zift :: Path Abs Dir -> IO (ZiftResult a)
    } deriving (Generic)

instance Functor Zift where
    fmap f (Zift iof) =
        Zift $ \rootdir -> do
            r <- iof rootdir
            pure $ fmap f r

instance Applicative Zift where
    pure a = Zift $ \_ -> pure $ pure a
    (Zift faf) <*> (Zift af) =
        Zift $ \rd -> do
            faa <- async $ faf rd
            aa <- async $ af rd
            fa <- wait faa -- TODO fail immediately if either already fails.
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
    liftIO act = Zift $ \_ -> do (ZiftSuccess <$> act) `catch` handler
      where
        handler :: SomeException -> IO (ZiftResult a)
        handler ex = pure $ ZiftFailed $ displayException ex

data ZiftResult a
    = ZiftSuccess a
    | ZiftFailed String
    deriving (Show, Eq, Generic)

instance Validity a =>
         Validity (ZiftResult a) where
    isValid (ZiftSuccess a) = isValid a
    isValid _ = True

instance Functor ZiftResult where
    fmap f (ZiftSuccess a) = ZiftSuccess $ f a
    fmap _ (ZiftFailed s) = ZiftFailed s

instance Applicative ZiftResult where
    pure a = ZiftSuccess a
    (ZiftSuccess f) <*> (ZiftSuccess a) = ZiftSuccess $ f a
    (ZiftFailed e) <*> (ZiftSuccess _) = ZiftFailed e
    (ZiftSuccess _) <*> (ZiftFailed e) = ZiftFailed e
    (ZiftFailed e1) <*> (ZiftFailed e2) = ZiftFailed $ unwords [e1, e2]

instance Monad ZiftResult where
    (ZiftSuccess a) >>= fb = fb a
    (ZiftFailed e) >>= _ = ZiftFailed e

instance MonadFail ZiftResult where
    fail = ZiftFailed
