{-# LANGUAGE FlexibleContexts #-}

module Zifter.StackSpec
    ( spec
    ) where

import TestImport

import Control.Concurrent.STM

import Zifter
import Zifter.OptParse
import Zifter.Stack
import Zifter.Zift

spec :: Spec
spec =
    describe "stackGetPackageTargetTuples" $
    it "finds the right packages for this repository" $ do
        tups <- runZiftInRepo stackGetPackageTargetTuples
        tups `shouldBe`
            [ ("zifter", ["zifter:lib", "zifter:test:zifter-test"])
            , ("zifter-cabal", ["zifter-cabal:lib"])
            , ("zifter-git", ["zifter-git:lib"])
            , ("zifter-google-java-format", ["zifter-google-java-format:lib"])
            , ("zifter-hindent", ["zifter-hindent:lib"])
            , ("zifter-hlint", ["zifter-hlint:lib"])
            , ( "zifter-stack"
              , ["zifter-stack:lib", "zifter-stack:test:zifter-stack-test"])
            ]

runZiftInRepo :: Zift a -> IO a
runZiftInRepo func = do
    rd <- resolveDir' ".."
    pchan <- newTChanIO
    td <- resolveDir rd "/tmp/zifter-tmp"
    let ctx =
            ZiftContext
            { rootdir = rd
            , tmpdir = td
            , settings =
                  Settings
                  {setsOutputColor = False, setsOutputMode = OutputFast}
            , printChan = pchan
            , recursionList = []
            }
    zr <- runZift ctx func
    case zr of
        ZiftSuccess a -> pure a
        ZiftFailed r -> do
            expectationFailure $ "zift failed: " ++ show r
            undefined -- won't get here anyway
