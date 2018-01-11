{-# OPTIONS_GHC -Wno-orphans #-}

module Zifter.Gen where

import TestImport

import Zifter
import Zifter.Zift.Gen ()

instance GenUnchecked LinearState
