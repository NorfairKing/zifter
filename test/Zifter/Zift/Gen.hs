module Zifter.Zift.Gen where

import TestIntroduction

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
