{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Zifter.ZiftSpec
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

import Zifter.OptParse.Gen ()
import Zifter.OptParse.Types
import Zifter.Zift
import Zifter.Zift.Gen ()

{-# ANN module "HLint: ignore Reduce duplication" #-}

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
    describe "Zift" $ do
        describe "Monoid Zift" $ do
            describe "mempty" $
                it "succeeds with empty buffer" $
                forAll genUnchecked $ \sets -> do
                    let zf = mempty :: Zift ()
                    (zr, zs) <- runZiftTest sets zf
                    zr `shouldBe` ZiftSuccess ()
                    zs `shouldBe` ZiftState {bufferedOutput = []}
            describe "mappend" $
                it
                    "succeeds with empty buffers but with the output in the channel" $
                forAll genUnchecked $ \sets ->
                    forAll genUnchecked $ \zo1 ->
                        forAll genUnchecked $ \zo2 -> do
                            let zf1 =
                                    Zift $ \_ _ ->
                                        pure
                                            ( ZiftSuccess ()
                                            , ZiftState {bufferedOutput = [zo1]})
                            let zf2 =
                                    Zift $ \_ _ ->
                                        pure
                                            ( ZiftSuccess ()
                                            , ZiftState {bufferedOutput = [zo2]})
                            let zf = zf1 `mappend` zf2
                            pchan <- atomically newTChan
                            (zr, zs) <- runZiftTestWithChan pchan sets zf
                            zr `shouldBe` ZiftSuccess ()
                            zs `shouldBe` ZiftState {bufferedOutput = []}
                            atomically (readTChan pchan) `shouldReturn` zo1
                            atomically (readTChan pchan) `shouldReturn` zo2
        describe "Functor Zift" $
            describe "fmap" $
            it "it succeeds with a flushed buffer" $
            forAll genUnchecked $ \sets ->
                forAll genUnchecked $ \state -> do
                    let zf =
                            fmap (+ 1) $
                            Zift $ \_ st -> pure (ZiftSuccess (0 :: Int), st)
                    pchan <- atomically newTChan
                    (zr, zs) <- runZiftTestWith state pchan sets zf
                    zr `shouldBe` ZiftSuccess 1
                    zs `shouldBe` ZiftState {bufferedOutput = []}
                    buffer <- readAllFrom pchan
                    buffer `shouldBe` reverse (bufferedOutput state)
        describe "Applicative Zift" $ do
            describe "pure" $
                it "succeeds with the same state" $
                forAll genUnchecked $ \sets ->
                    forAll genUnchecked $ \state -> do
                        let zf = pure () :: Zift ()
                        (zr, zs) <- runZiftTestWithState state sets zf
                        zr `shouldBe` ZiftSuccess ()
                        zs `shouldBe` state
            describe "<*>" $ do
                it
                    "succeeds with a linear flushed buffer if used at the top-level" $
                    forAll genUnchecked $ \sets ->
                        forAll genUnchecked $ \state ->
                            forAll genUnchecked $ \st1 ->
                                forAll genUnchecked $ \st2 -> do
                                    let zf1 =
                                            Zift $ \_ st ->
                                                pure
                                                    ( ZiftSuccess (+ 1)
                                                    , st `mappend` st1)
                                    let zf2 =
                                            Zift $ \_ st ->
                                                pure
                                                    ( ZiftSuccess (1 :: Int)
                                                    , st `mappend` st2)
                                    let zf = zf1 <*> zf2
                                    pchan <- atomically newTChan
                                    (zr, zs) <-
                                        runZiftTestWith state pchan sets zf
                                    zr `shouldBe` ZiftSuccess 2
                                    zs `shouldBe`
                                        ZiftState {bufferedOutput = []}
                                    buffer <- readAllFrom pchan
                                    buffer `shouldBe`
                                        reverse
                                            (bufferedOutput st2 ++
                                             bufferedOutput st1 ++
                                             bufferedOutput state)
                it "fails immediately if the first of the two actions failed" $
                    forAll genUnchecked $ \sets ->
                        forAll genUnchecked $ \state ->
                            forAll genUnchecked $ \st1 ->
                                forAll genUnchecked $ \failedMessage -> do
                                    let zf1 =
                                            Zift $ \_ st ->
                                                pure
                                                    ( ZiftFailed failedMessage :: ZiftResult (Int -> String)
                                                    , st `mappend` st1)
                                    let zf2 = do
                                            liftIO $
                                                threadDelay $ 5 * 1000 * 1000
                                            pure 1 :: Zift Int
                                    let zf = zf1 <*> zf2
                                    pchan <- atomically newTChan
                                    (zr, zs) <-
                                        runZiftTestWith state pchan sets zf
                                    zr `shouldBe`
                                        (ZiftFailed failedMessage :: ZiftResult String)
                                    zs `shouldBe`
                                        ZiftState
                                        { bufferedOutput =
                                              bufferedOutput st1 ++
                                              bufferedOutput state
                                        }
                                    buffer <- readAllFrom pchan
                                    buffer `shouldBe` []
                it "fails immediately if the second of the two actions failed" $
                    forAll genUnchecked $ \sets ->
                        forAll genUnchecked $ \state ->
                            forAll genUnchecked $ \st2 ->
                                forAll genUnchecked $ \failedMessage -> do
                                    let zf1 = do
                                            liftIO $
                                                threadDelay $ 5 * 1000 * 1000
                                            pure show :: Zift (Int -> String)
                                    let zf2 =
                                            Zift $ \_ st ->
                                                pure
                                                    ( ZiftFailed failedMessage :: ZiftResult Int
                                                    , st `mappend` st2)
                                    let zf = zf1 <*> zf2
                                    pchan <- atomically newTChan
                                    (zr, zs) <-
                                        runZiftTestWith state pchan sets zf
                                    zr `shouldBe`
                                        (ZiftFailed failedMessage :: ZiftResult String)
                                    zs `shouldBe`
                                        ZiftState
                                        { bufferedOutput =
                                              bufferedOutput st2 ++
                                              bufferedOutput state
                                        }
                                    buffer <- readAllFrom pchan
                                    buffer `shouldBe` []
                it
                    "Orders the messages correctly when they are printed in a for_ loop" $
                    forAll genUnchecked $ \ls ->
                        forAll genUnchecked $ \rl ->
                            forAll genUnchecked $ \sets -> do
                                let zf =
                                        withRecursionList rl $
                                        for_ ls addZiftOutput
                                pchan <- atomically newTChan
                                (zr, zs) <- runZiftTestWithChan pchan sets zf
                                zr `shouldBe` ZiftSuccess ()
                                buffer <- readAllFrom pchan
                                (buffer ++ reverse (bufferedOutput zs)) `shouldBe`
                                    ls
        describe "Monad Zift" $
            describe ">>=" $ do
                it "succeeds with a flushed buffer after the first output" $
                    forAll genUnchecked $ \sets ->
                        forAll genUnchecked $ \state ->
                            forAll genUnchecked $ \st1 ->
                                forAll genUnchecked $ \st2 -> do
                                    let zf1 =
                                            Zift $ \_ st ->
                                                pure
                                                    ( ZiftSuccess (2 :: Int)
                                                    , st `mappend` st1)
                                    let zf2 n =
                                            Zift $ \_ st ->
                                                pure
                                                    ( ZiftSuccess (n + 1)
                                                    , st `mappend` st2)
                                    let zf = zf1 >>= zf2
                                    pchan <- atomically newTChan
                                    (zr, zs) <-
                                        runZiftTestWith state pchan sets zf
                                    zr `shouldBe` ZiftSuccess 3
                                    zs `shouldBe` st2
                                    buffer <- readAllFrom pchan
                                    buffer `shouldBe`
                                        reverse
                                            (bufferedOutput st1 ++
                                             bufferedOutput state)
                it "fails if the first part failed" $
                    forAll genUnchecked $ \sets ->
                        forAll genUnchecked $ \state ->
                            forAll genUnchecked $ \st1 ->
                                forAll genUnchecked $ \st2 ->
                                    forAll genUnchecked $ \failMsg -> do
                                        let zf1 =
                                                Zift $ \_ st ->
                                                    pure
                                                        ( ZiftFailed failMsg :: ZiftResult Int
                                                        , st `mappend` st1)
                                        let zf2 n =
                                                Zift $ \_ st ->
                                                    pure
                                                        ( ZiftSuccess (n + 1)
                                                        , st `mappend` st2)
                                        let zf = zf1 >>= zf2
                                        pchan <- atomically newTChan
                                        (zr, zs) <-
                                            runZiftTestWith state pchan sets zf
                                        zr `shouldBe` ZiftFailed failMsg
                                        zs `shouldBe` mempty
                                        buffer <- readAllFrom pchan
                                        buffer `shouldBe`
                                            reverse
                                                (bufferedOutput st1 ++
                                                 bufferedOutput state)
                it "fails if the second part failed" $
                    forAll genUnchecked $ \sets ->
                        forAll genUnchecked $ \state ->
                            forAll genUnchecked $ \st1 ->
                                forAll genUnchecked $ \st2 ->
                                    forAll genUnchecked $ \failMsg -> do
                                        let zf1 =
                                                Zift $ \_ st ->
                                                    pure
                                                        ( ZiftSuccess (1 :: Int)
                                                        , st `mappend` st1)
                                        let zf2 _ =
                                                Zift $ \_ st ->
                                                    pure
                                                        ( ZiftFailed failMsg :: ZiftResult (Int -> Int)
                                                        , st `mappend` st2)
                                        let zf = zf1 >>= zf2
                                        pchan <- atomically newTChan
                                        (zr, zs) <-
                                            runZiftTestWith state pchan sets zf
                                        case zr of
                                            ZiftSuccess _ ->
                                                expectationFailure
                                                    "should have failed."
                                            ZiftFailed msg -> do
                                                msg `shouldBe` failMsg
                                                zs `shouldBe` st2
                                                buffer <- readAllFrom pchan
                                                buffer `shouldBe`
                                                    reverse
                                                        (bufferedOutput st1 ++
                                                         bufferedOutput state)
                it
                    "Orders the messages correctly when they are printed in a forM_ loop at any depth" $
                    forAll genUnchecked $ \ls ->
                        forAll genUnchecked $ \rl ->
                            forAll genUnchecked $ \sets -> do
                                let zf = forM_ ls addZiftOutput
                                    zf' = withRecursionList rl zf
                                pchan <- atomically newTChan
                                (zr, zs) <- runZiftTestWithChan pchan sets zf'
                                zr `shouldBe` ZiftSuccess ()
                                buffer <- readAllFrom pchan
                                (buffer ++ reverse (bufferedOutput zs)) `shouldBe`
                                    ls
                it "Orders the messages in this do-notation correctly" $
                    forAll genUnchecked $ \(m1, m2, m3) ->
                        forAll genUnchecked $ \rl ->
                            forAll genUnchecked $ \sets -> do
                                let zf =
                                        withRecursionList rl $ do
                                            addZiftOutput m1
                                            addZiftOutput m2
                                            addZiftOutput m3
                                pchan <- atomically newTChan
                                (zr, zs) <- runZiftTestWithChan pchan sets zf
                                zr `shouldBe` ZiftSuccess ()
                                buffer <- readAllFrom pchan
                                (buffer ++ reverse (bufferedOutput zs)) `shouldBe`
                                    [m1, m2, m3]
        describe "MonadFail Zift" $ do
            describe "fail" $
                it "just results in a ZiftFailed" $
                forAll genUnchecked $ \sets ->
                    forAll genUnchecked $ \state -> do
                        let s = "Test"
                        let zf = fail s :: Zift ()
                        (zr, zs) <- runZiftTestWithState state sets zf
                        zr `shouldBe` ZiftFailed s
                        zs `shouldBe` state
            describe "liftIO . fail" $
                it "just returns ZiftFailed" $
                forAll genUnchecked $ \sets ->
                    forAll genUnchecked $ \state -> do
                        let s = "Test"
                        let zf = liftIO $ fail s :: Zift ()
                        (zr, zs) <- runZiftTestWithState state sets zf
                        zr `shouldBe` ZiftFailed ("user error (" ++ s ++ ")")
                        zs `shouldBe` state
        describe "tryFlushZiftBuffer" $ do
            it "does not do anything if there recursion list is not flushable" $
                forAll genUnchecked $ \state ->
                    forAllCtx $ \ctx ->
                        if flushable $ recursionList ctx
                            then pure () -- Not testing this part now.
                            else do
                                state' <- tryFlushZiftBuffer ctx state
                                state' `shouldBe` state
            it
                "flushes the entire buffer in the correct order, if the recursion list is empty" $
                forAll genUnchecked $ \state ->
                    forAllCtx $ \ctx ->
                        if flushable $ recursionList ctx
                            then do
                                state' <- tryFlushZiftBuffer ctx state
                                state' `shouldBe` state {bufferedOutput = []}
                                res <- readAllFrom $ printChan ctx
                                res `shouldBe` reverse (bufferedOutput state)
                            else pure ()

forAllCtx :: Testable (IO b) => (ZiftContext -> IO b) -> Property
forAllCtx func =
    forAll genUnchecked $ \(rd, sets, rl) -> do
        pchan <- atomically newTChan
        let zc =
                ZiftContext
                { rootdir = rd
                , settings = sets
                , printChan = pchan
                , recursionList = rl
                }
        func zc

runZiftTest :: Settings -> Zift a -> IO (ZiftResult a, ZiftState)
runZiftTest sets func = do
    pchan <- atomically newTChan
    runZiftTestWithChan pchan sets func

runZiftTestWithChan ::
       TChan ZiftOutput -> Settings -> Zift a -> IO (ZiftResult a, ZiftState)
runZiftTestWithChan = runZiftTestWith ZiftState {bufferedOutput = []}

runZiftTestWithState ::
       ZiftState -> Settings -> Zift a -> IO (ZiftResult a, ZiftState)
runZiftTestWithState state sets func = do
    pchan <- atomically newTChan
    runZiftTestWith state pchan sets func

runZiftTestWith ::
       ZiftState
    -> TChan ZiftOutput
    -> Settings
    -> Zift a
    -> IO (ZiftResult a, ZiftState)
runZiftTestWith zs pchan sets func = do
    rd <- getCurrentDir
    let zc =
            ZiftContext
            { rootdir = rd
            , settings = sets
            , printChan = pchan
            , recursionList = []
            }
    zift func zc zs

readAllFrom :: TChan a -> IO [a]
readAllFrom chan = do
    mr <- atomically $ tryReadTChan chan
    case mr of
        Nothing -> pure []
        Just r -> do
            rest <- readAllFrom chan
            pure (r : rest)

withRecursionList :: [LMR] -> Zift a -> Zift a
withRecursionList rl zf =
    zf {zift = \zc zs -> zift zf (zc {recursionList = rl}) zs}
