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
        it "pure () outputs nothing" $
            pure () `outputShouldBe` [ZiftToken [] Nothing]
        it "pure () twice outputs two tokens" $
            let func = do
                    pure ()
                    pure ()
            in func `outputShouldBe`
               [ZiftToken [L] Nothing, ZiftToken [R] Nothing]
        it "printZift outputs one message" $
            printZift "hello" `outputShouldBe`
            [ ZiftToken
                  []
                  (Just ZiftOutput {outputColors = [], outputMessage = "hello"})
            ]
        it "printZift twice outputs two messages and two tokens" $
            let func = do
                    printZift "hello"
                    printZift "world"
            in func `outputShouldBe`
               [ ZiftToken
                     [L]
                     (Just
                          ZiftOutput
                          {outputColors = [], outputMessage = "hello"})
               , ZiftToken
                     [R]
                     (Just
                          ZiftOutput
                          {outputColors = [], outputMessage = "world"})
               ]
    describe "addState" $ do
        it "stores the first output on the left for [L]" $
            addState LinearUnknown (ZiftToken [L] Nothing) `shouldBe`
            LinearBranch (LinearLeaf Nothing) LinearUnknown
        it "stores the first output on the Right for [R]" $
            addState LinearUnknown (ZiftToken [R] Nothing) `shouldBe`
            LinearBranch LinearUnknown (LinearLeaf Nothing)
    describe "flushState" $ do
        it "flushes a simple branch at the top level" $
            forAllUnchecked $ \(hello, world) ->
                flushState
                    (LinearBranch
                         (LinearLeaf (Just hello))
                         (LinearLeaf (Just world))) `shouldBe`
                (LinearLeaf Nothing, [hello, world])
        it
            "flushes and prunes the left side of a branch if the right side is unknown" $
            forAllUnchecked $ \msg ->
                let s = LinearBranch (LinearLeaf (Just msg)) LinearUnknown
                in flushState s `shouldBe` (LinearUnknown, [msg])
        it
            "does not flush the right side of a branch if the left side is unknown" $
            forAllUnchecked $ \msg ->
                let s = LinearBranch LinearUnknown (LinearLeaf (Just msg))
                in flushState s `shouldBe` (s, [])
        it
            "flushes the entire state when the left side is done and the right side is one level deep" $
            forAllUnchecked $ \(hello, world) ->
                let s =
                        LinearBranch
                            (LinearLeaf Nothing)
                            (LinearBranch
                                 (LinearLeaf (Just hello))
                                 (LinearLeaf (Just world)))
                in flushState s `shouldBe` (LinearLeaf Nothing, [hello, world])
        it
            "flushes the entire state when the left side is done and the right side is two levels deep" $
            forAllUnchecked $ \(hello, big, beautiful, world) ->
                let s =
                        LinearBranch
                            (LinearLeaf Nothing)
                            (LinearBranch
                                 (LinearBranch
                                      (LinearLeaf (Just hello))
                                      (LinearLeaf (Just big)))
                                 (LinearBranch
                                      (LinearLeaf (Just beautiful))
                                      (LinearLeaf (Just world))))
                in flushState s `shouldBe`
                   (LinearLeaf Nothing, [hello, big, beautiful, world])
        it
            "flushes and prunes the entire left half of a complete binary tree of size two if the entire left part is done" $
            forAllUnchecked $ \(hello, world) ->
                let s =
                        LinearBranch
                            (LinearBranch
                                 (LinearLeaf (Just hello))
                                 (LinearLeaf (Just world)))
                            (LinearBranch LinearUnknown LinearUnknown)
                    s' = LinearBranch LinearUnknown LinearUnknown
                in flushState s `shouldBe` (s', [hello, world])
        it
            "flushes and prunes the correct part of the right half of the state when the left part is done and the right side isn't" $
            forAllUnchecked $ \(hello, world) ->
                let s =
                        LinearBranch
                            (LinearLeaf (Just hello))
                            (LinearBranch
                                 (LinearLeaf (Just world))
                                 LinearUnknown)
                    s' = LinearUnknown
                in flushState s `shouldBe` (s', [hello, world])
        it
            "flushes and prunes the entire left half of a complete binary tree of size two if the entire left part is done" $
            forAllUnchecked $ \(hello, beautiful, world) ->
                let s =
                        LinearBranch
                            (LinearBranch
                                 (LinearLeaf (Just hello))
                                 (LinearLeaf (Just beautiful)))
                            (LinearBranch
                                 (LinearLeaf (Just world))
                                 LinearUnknown)
                    s' = LinearUnknown
                in flushState s `shouldBe` (s', [hello, beautiful, world])

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
