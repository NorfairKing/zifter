{-# OPTIONS_GHC -Wno-orphans #-}

module Zifter.Gen where

import Data.GenValidity

import Zifter
import Zifter.Zift.Gen ()

instance GenUnchecked LinearState
