{-# LANGUAGE FlexibleContexts #-}

module ZifterSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Path.IO

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.Foldable
import Data.GenValidity.Path ()
import System.Directory (withCurrentDirectory)

import Zifter
import Zifter.OptParse.Gen ()
import Zifter.OptParse.Types
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
            runZift ctx $ pure ()
