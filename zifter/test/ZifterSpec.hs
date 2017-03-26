{-# LANGUAGE FlexibleContexts #-}

module ZifterSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Control.Concurrent.STM
import Data.GenValidity.Path ()
import System.Exit (ExitCode(..))

import Zifter
import Zifter.OptParse.Gen ()
import Zifter.Zift
import Zifter.Zift.Gen ()

spec :: Spec
spec =
    describe "ziftWith" $
    it "does nothing with an empty zift action" $
    forAll genUnchecked $ \sets ->
        forAll genValid $ \rd -> do
            pchan <- newTChanIO
            let ctx =
                    ZiftContext
                    { rootdir = rd
                    , settings = sets
                    , printChan = pchan
                    , recursionList = []
                    }
            runZift ctx (pure ()) `shouldReturn` ExitSuccess
