{-# OPTIONS_GHC -Wno-orphans #-}

module Zifter.OptParse.Gen where

import TestImport

import Zifter.OptParse.Types

instance GenUnchecked OutputMode

instance GenUnchecked Settings
