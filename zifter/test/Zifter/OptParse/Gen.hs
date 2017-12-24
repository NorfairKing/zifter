{-# OPTIONS_GHC -Wno-orphans #-}

module Zifter.OptParse.Gen where

import Zifter.OptParse.Types

import Test.Validity

instance GenUnchecked OutputMode

instance GenUnchecked Settings
