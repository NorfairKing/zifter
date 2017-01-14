{-# LANGUAGE DeriveGeneric #-}

module Zifter.PreProcess.Types where

import Introduction

import Control.Monad.Fail

data PreProcessor a = PreProcessor
    { preprocess :: Path Abs Dir -> IO (PreProcessorResult a)
    } deriving (Generic)

instance Functor PreProcessor where
    fmap f (PreProcessor iof) =
        PreProcessor $ \rootdir -> do
            r <- iof rootdir
            pure $ fmap f r

instance Applicative PreProcessor where
    pure a = PreProcessor $ \_ -> pure $ pure a
    (PreProcessor faf) <*> (PreProcessor af) =
        PreProcessor $ \rd -> do
            faa <- async $ faf rd
            aa <- async $ af rd
            fa <- wait faa -- TODO fail immediately if either already fails.
            a <- wait aa
            pure $ fa <*> a

instance Monad PreProcessor where
    (PreProcessor fa) >>= mb =
        PreProcessor $ \rd -> do
            ra <- fa rd
            case ra of
                PreProcessorSuccess a ->
                    case mb a of
                        PreProcessor pb -> pb rd
                PreProcessorFailed e -> pure $ PreProcessorFailed e

instance MonadFail PreProcessor where
    fail s = PreProcessor $ \_ -> pure $ fail s

instance MonadIO PreProcessor where
    liftIO act =
        PreProcessor $ \_ -> do (PreProcessorSuccess <$> act) `catch` handler
      where
        handler :: SomeException -> IO (PreProcessorResult a)
        handler ex = pure $ PreProcessorFailed $ displayException ex

data PreProcessorResult a
    = PreProcessorSuccess a
    | PreProcessorFailed String
    deriving (Show, Eq, Generic)

instance Validity a =>
         Validity (PreProcessorResult a) where
    isValid (PreProcessorSuccess a) = isValid a
    isValid _ = True

instance Functor PreProcessorResult where
    fmap f (PreProcessorSuccess a) = PreProcessorSuccess $ f a
    fmap _ (PreProcessorFailed s) = PreProcessorFailed s

instance Applicative PreProcessorResult where
    pure a = PreProcessorSuccess a
    (PreProcessorSuccess f) <*> (PreProcessorSuccess a) =
        PreProcessorSuccess $ f a
    (PreProcessorFailed e) <*> (PreProcessorSuccess _) = PreProcessorFailed e
    (PreProcessorSuccess _) <*> (PreProcessorFailed e) = PreProcessorFailed e
    (PreProcessorFailed e1) <*> (PreProcessorFailed e2) =
        PreProcessorFailed $ unwords [e1, e2]

instance Monad PreProcessorResult where
    (PreProcessorSuccess a) >>= fb = fb a
    (PreProcessorFailed e) >>= _ = PreProcessorFailed e

instance MonadFail PreProcessorResult where
    fail = PreProcessorFailed
