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
    functorSpec @ZiftResult
