{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Zifter.ZiftSpec
    ( spec
    ) where

import TestImport

import Control.Concurrent.STM

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
            forAllUnchecked $ \mzo ->
                addState LinearUnknown (ZiftToken [L] mzo) `shouldBe`
                Just (LinearBranch (LinearLeaf mzo) LinearUnknown)
        it "stores the first output on the Right for [R]" $
            forAllUnchecked $ \mzo ->
                addState LinearUnknown (ZiftToken [R] mzo) `shouldBe`
                Just (LinearBranch LinearUnknown (LinearLeaf mzo))
    describe "flushState" $ do
        let l = LinearLeaf
            u = LinearUnknown
            d = LinearDone
            b = LinearBranch
            ln = l Nothing
            t bs es eb =
                let (as, ab) = flushState bs
                in do as `shouldBe` es
                      ab `shouldBe` eb
        it "flushes a simple branch at the top level" $
            forAllUnchecked $ \(hello, world) ->
                t
                    (b (l (Just hello)) (l (Just world)))
                    (b d d)
                    (BufReady [hello, world])
        it
            "flushes and prunes the left side of a branch if the right side is unknown" $
            forAllUnchecked $ \msg ->
                t (b (l (Just msg)) u) (b d u) (BufReady [msg])
        it
            "does not flush the right side of a branch if the left side is unknown" $
            forAllUnchecked $ \msg ->
                let s = b u (l (Just msg))
                in t s s BufNotReady
        it "flushes a branch with two leaves" $
            forAllUnchecked $ \(hello, world) ->
                t
                    (b (l (Just hello)) (l (Just world)))
                    (b d d)
                    (BufReady [hello, world])
        it
            "flushes the entire state when the left side is done and the right side is one level deep" $
            forAllUnchecked $ \(hello, world) ->
                t
                    (b ln (b (l (Just hello)) (l (Just world))))
                    (b d (b d d))
                    (BufReady [hello, world])
        it
            "flushes the entire state when the left side is done and the right side is two levels deep" $
            forAllUnchecked $ \(hello, big, beautiful, world) ->
                t
                    (b (l Nothing)
                         (b (b (l (Just hello)) (l (Just big)))
                              (b (l (Just beautiful)) (l (Just world)))))
                    (b d (b (b d d) (b d d)))
                    (BufReady [hello, big, beautiful, world])
        it
            "flushes the entire left half of a complete binary tree of size two if the entire left part is done" $
            forAllUnchecked $ \(hello, world) ->
                t
                    (b (b (l (Just hello)) (l (Just world))) (b u u))
                    (b (b d d) (b u u))
                    (BufReady [hello, world])
        it
            "flushes the correct part of the right half of the state when the left part is done and the right side isn't" $
            forAllUnchecked $ \(hello, world) ->
                t
                    (b (l (Just hello)) (b (l (Just world)) u))
                    (b d (b d u))
                    (BufReady [hello, world])
        it
            "flushes and the entire left half of a complete binary tree of size two if the entire left part is done" $
            forAllUnchecked $ \(hello, beautiful, world) ->
                t
                    (b (b (l (Just hello)) (l (Just beautiful)))
                         (b (l (Just world)) u))
                    (b (b d d) (b d u))
                    (BufReady [hello, beautiful, world])
        it "flushes the entire tree for any done tree" $
            forAll doneTree $ \st ->
                let (s', _) = flushState st
                in s' `shouldBe` makeForceFlushed st
        it "flushes the entire left tree for any tree whose left part is done" $
            forAllShrink doneTree (map makeForceFlushed . shrinkUnchecked) $ \dt ->
                forAllUnchecked $ \ut ->
                    let s = b dt ut
                        (rs', b2) = flushState ut
                    in t s
                           (b (makeForceFlushed dt) rs')
                           (flushStateAll dt <> b2)
        it "can only grow the depth of the state" $
            forAll
                (genUnchecked `suchThat`
                 (\(st, token) -> isJust $ processToken st token)) $ \(st, token) ->
                case processToken st token of
                    Nothing -> pure () -- fine
                    Just (t', _) -> depth t' `shouldSatisfy` (>= depth st)

depth :: LinearState -> Int
depth LinearUnknown = 1
depth LinearDone = 1
depth (LinearLeaf _) = 1
depth (LinearBranch t1 t2) = max (depth t1) (depth t2)

doneTree :: Gen LinearState
doneTree =
    sized $ \s ->
        oneof
            [ LinearLeaf <$> genUnchecked
            , pure LinearDone
            , do (ls, rs) <- genSplit s
                 LinearBranch <$> resize ls doneTree <*> resize rs doneTree
            ]

makeForceFlushed :: LinearState -> LinearState
makeForceFlushed LinearUnknown = LinearUnknown
makeForceFlushed LinearDone = LinearDone
makeForceFlushed (LinearLeaf _) = LinearDone
makeForceFlushed (LinearBranch s1 s2) =
    LinearBranch (makeForceFlushed s1) (makeForceFlushed s2)

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
