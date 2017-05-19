{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Zifter.Zift.Gen where

import GHC.Generics

import Data.GenValidity
import Data.GenValidity.Containers

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

instance GenUnchecked RecursionPath

instance GenUnchecked ZiftOutput

instance GenUnchecked a =>
         GenUnchecked (ZiftResult a)

instance GenValid a =>
         GenValid (ZiftResult a) where
    genValid = ZiftSuccess <$> genValid

instance GenInvalid a =>
         GenInvalid (ZiftResult a) where
    genInvalid = ZiftSuccess <$> genInvalid

instance GenUnchecked OutputBuffer

instance GenUnchecked OutputRecord

instance GenUnchecked BookkeeperState
