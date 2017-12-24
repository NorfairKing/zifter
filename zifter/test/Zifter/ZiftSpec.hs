{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Zifter.ZiftSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity

import Data.GenValidity.Path ()

import Zifter.Zift

import Zifter.OptParse.Gen ()
import Zifter.Zift.Gen ()

spec :: Spec
spec = do
    describe "ZiftOutput" $ eqSpec @ZiftOutput
    describe "ZiftResult" $ do
        eqSpec @(ZiftResult Int)
        genValiditySpec @(ZiftResult Double)
        functorSpec @ZiftResult
        applicativeSpec @ZiftResult
        monoidSpec @(ZiftResult String)
        monadSpec @ZiftResult
