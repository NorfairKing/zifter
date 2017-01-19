{-# OPTIONS_GHC -Wno-orphans #-}

module Zifter.Zift.Gen where

import Data.GenValidity

import Zifter.Zift.Types

instance GenUnchecked a =>
         GenUnchecked (ZiftResult a) where
    genUnchecked = ZiftSuccess <$> genUnchecked

instance GenValid a =>
         GenValid (ZiftResult a) where
    genValid = ZiftSuccess <$> genValid

instance GenInvalid a =>
         GenInvalid (ZiftResult a) where
    genInvalid = ZiftSuccess <$> genInvalid
