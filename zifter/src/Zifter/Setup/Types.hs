{-# LANGUAGE DeriveGeneric #-}

module Zifter.Setup.Types where

import GHC.Generics

import Zifter.Zift.Types

data ZiftSetup = ZiftSetup
    { ziftPreprocessor :: Zift ()
    , ziftPreCheckHook :: Zift ()
    , ziftChecker :: Zift ()
    } deriving (Generic)

instance Monoid ZiftSetup where
    mempty =
        ZiftSetup
        { ziftPreprocessor = pure ()
        , ziftPreCheckHook = pure ()
        , ziftChecker = pure ()
        }
    mappend z1 z2 =
        ZiftSetup
        { ziftPreprocessor = ziftPreprocessor z1 `mappend` ziftPreprocessor z2
        , ziftPreCheckHook = ziftPreCheckHook z1 `mappend` ziftPreCheckHook z2
        , ziftChecker = ziftChecker z1 `mappend` ziftChecker z2
        }
