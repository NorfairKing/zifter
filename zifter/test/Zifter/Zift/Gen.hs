{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Zifter.Zift.Gen where

import GHC.Generics

import Data.Colour.SRGB
import Data.GenValidity
import Data.Monoid

import System.Console.ANSI

import Zifter.Zift.Types

deriving instance Generic Underlining

instance GenUnchecked Underlining

deriving instance Generic BlinkSpeed

instance GenUnchecked BlinkSpeed

deriving instance Generic ConsoleLayer

instance GenUnchecked ConsoleLayer

deriving instance Generic Color

instance GenUnchecked Color

deriving instance Generic ConsoleIntensity

instance GenUnchecked ConsoleIntensity

deriving instance Generic ColorIntensity

instance GenUnchecked ColorIntensity

deriving instance Generic SGR

instance GenUnchecked SGR

instance GenUnchecked LR

instance GenUnchecked ZiftToken

instance GenUnchecked ZiftOutput

instance (Validity a) => Validity (RGB a) where
    validate RGB {..} =
        delve "channelRed" channelRed <> delve "channelGreen" channelGreen <>
        delve "channelBlue" channelBlue

instance (Ord a, Floating a, GenUnchecked a) => GenUnchecked (Colour a) where
    genUnchecked = sRGB24 <$> genUnchecked <*> genUnchecked <*> genUnchecked
    shrinkUnchecked _ = []

instance (Floating a, RealFrac a, Validity a) => Validity (Colour a) where
    validate = delve "Colour" . toSRGB24

instance (Ord a, Floating a, RealFrac a, GenValid a) =>
         GenValid (Colour a) where
    genValid = sRGB24 <$> genValid <*> genValid <*> genValid

instance GenUnchecked a => GenUnchecked (ZiftResult a) where
    genUnchecked = ZiftSuccess <$> genUnchecked

instance GenValid a => GenValid (ZiftResult a) where
    genValid = ZiftSuccess <$> genValid

instance GenInvalid a => GenInvalid (ZiftResult a) where
    genInvalid = ZiftSuccess <$> genInvalid
