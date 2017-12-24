{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Zifter.ZiftSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity

import Data.GenValidity.Path ()

import Control.Concurrent.STM

import Path.IO

import Zifter
import Zifter.OptParse
import Zifter.Zift

import Zifter.Gen ()
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
    describe "ziftRunner" $ do
        it "pure () outputs nothing" $ pure () `outputShouldBe` [
                 TokenDone []
                ]
        it "pure () twice outputs two tokens" $
            let func = do
                    pure ()
                    pure ()
            in func `outputShouldBe` [TokenDone [L], TokenDone [R]
                , TokenDone []
                ]
        it "printZift outputs one message" $
            printZift "hello" `outputShouldBe`
            [ TokenOutput
                  []
                  ZiftOutput {outputColors = [], outputMessage = "hello"}
            , TokenDone []
            ]
        it "printZift twice outputs two messages and two tokens" $
            let func = do
                    printZift "hello"
                    printZift "world"
            in func `outputShouldBe`
               [ TokenOutput
                     [L]
                     ZiftOutput {outputColors = [], outputMessage = "hello"}
               , TokenDone [L]
               , TokenOutput
                     [R]
                     ZiftOutput {outputColors = [], outputMessage = "world"}
               , TokenDone [R]
                , TokenDone []
               ]
    describe "addState" $ do
        it "flushes nothing on a print token" $ do
            forAllUnchecked $ \st ->
                forAllUnchecked $ \lr ->
                    let (st', zos) = addState st (TokenDone lr)
                    in zos `shouldBe` []

outputShouldBe :: Zift () -> [ZiftToken] -> Expectation
outputShouldBe func ls = outputShouldSatisfy func (== ls)

outputShouldSatisfy :: Zift () -> ([ZiftToken] -> Bool) -> Expectation
outputShouldSatisfy func predicate = do
    rd <- resolveDir' "/tmp/zifter"
    td <- resolveDir rd ".zifter"
    pchan <- newTChanIO
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
    fmvar <- newEmptyTMVarIO
    ec <- ziftRunner ctx fmvar func
    ec `shouldBe` ZiftSuccess ()
    atomically (takeTMVar fmvar) `shouldReturn` ()
    outs <- readAllFrom pchan
    outs `shouldSatisfy` predicate

readAllFrom :: TChan a -> IO [a]
readAllFrom chan = do
    mr <- atomically $ tryReadTChan chan
    case mr of
        Nothing -> pure []
        Just r -> do
            rest <- readAllFrom chan
            pure (r : rest)
