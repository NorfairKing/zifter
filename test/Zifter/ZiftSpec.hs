{-# LANGUAGE TypeApplications #-}

module Zifter.ZiftSpec
    ( spec
    ) where

import TestIntroduction

import Zifter.Zift

import Zifter.Zift.Gen ()

spec :: Spec
spec = do
    eqSpec @(ZiftResult Int)
    genValiditySpec @(ZiftResult Double)
    functorSpec @ZiftResult
    applicativeSpec @ZiftResult
    monadSpec @ZiftResult
