{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Zifter.Gen where

import GHC.Generics

import Data.Colour.SRGB
import Data.GenValidity
import Data.Monoid

import Zifter.Zift.Gen
import Zifter

instance GenUnchecked LinearState
