module Zifter.Zift.Gen where

import TestIntroduction

import Zifter.Zift.Types

instance GenUnchecked a =>
         GenUnchecked (ZiftResult a) where
    genUnchecked = ZiftSuccess <$> genUnchecked
