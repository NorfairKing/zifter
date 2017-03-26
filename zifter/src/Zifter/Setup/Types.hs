{-# LANGUAGE DeriveGeneric #-}

module Zifter.Setup.Types where

import GHC.Generics

import Zifter.Zift.Types

data ZiftSetup = ZiftSetup
    { ziftPreprocessor :: Zift ()
    , ziftPreChecker :: Zift ()
    , ziftChecker :: Zift ()
    } deriving (Generic)

instance Monoid ZiftSetup where
    mempty =
        ZiftSetup
        { ziftPreprocessor = pure ()
        , ziftPreChecker = pure ()
        , ziftChecker = pure ()
        }
    mappend z1 z2 =
        ZiftSetup
        { ziftPreprocessor = ziftPreprocessor z1 `mappend` ziftPreprocessor z2
        , ziftPreChecker = ziftPreChecker z1 `mappend` ziftPreChecker z2
        , ziftChecker = ziftChecker z1 `mappend` ziftChecker z2
        }
