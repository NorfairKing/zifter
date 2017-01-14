{-# LANGUAGE TypeApplications #-}

module Zifter.PreProcessSpec
    ( spec
    ) where

import TestIntroduction

import Zifter.PreProcess

import Zifter.PreProcess.Gen ()

spec :: Spec
spec = do
    eqSpec @(PreProcessorResult Int)
    functorSpec @PreProcessorResult
