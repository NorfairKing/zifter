module Zifter.PreProcess.Gen where

import TestIntroduction

import Zifter.PreProcess.Types

instance GenUnchecked a =>
         GenUnchecked (PreProcessorResult a) where
    genUnchecked = PreProcessorSuccess <$> genUnchecked
