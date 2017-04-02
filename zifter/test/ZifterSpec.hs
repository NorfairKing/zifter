{-# LANGUAGE FlexibleContexts #-}

module ZifterSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Path.IO

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
            td <- resolveDir rd ".zifter"
            let ctx =
                    ZiftContext
                    { rootdir = rd
                    , tmpdir = td
                    , settings = sets
                    , printChan = pchan
                    , recursionList = []
                    }
            runZift ctx (pure ()) `shouldReturn` ExitSuccess
