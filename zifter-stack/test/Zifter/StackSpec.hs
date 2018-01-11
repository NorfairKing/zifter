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
        tups <- runZiftInRepo stackGetPackages
        tups `shouldBe`
            [ Pkg "zifter" [Lib "zifter:lib", Test "zifter:test:zifter-test"]
            , Pkg "zifter-cabal" [Lib "zifter-cabal:lib"]
            , Pkg "zifter-git" [Lib "zifter-git:lib"]
            , Pkg "zifter-google-java-format"
                  [Lib "zifter-google-java-format:lib"]
            , Pkg "zifter-hindent" [Lib "zifter-hindent:lib"]
            , Pkg "zifter-hlint" [Lib "zifter-hlint:lib"]
            , Pkg "zifter-stack"
                  [ Lib "zifter-stack:lib"
                  , Test "zifter-stack:test:zifter-stack-test"
                  ]
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
