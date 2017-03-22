{-# LANGUAGE TypeApplications #-}

module Zifter.ZiftSpec
    ( spec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Path.IO

import Control.Concurrent.STM

import Zifter.OptParse.Gen ()
import Zifter.OptParse.Types
import Zifter.Zift
import Zifter.Zift.Gen ()

spec :: Spec
spec = do
    describe "ZiftOutput" $ eqSpec @ZiftOutput
    describe "ZiftResult" $ do
        eqSpec @(ZiftResult Int)
        genValiditySpec @(ZiftResult Double)
        functorSpec @ZiftResult
        applicativeSpec @ZiftResult
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
            describe "<*>" $
                it "succeeds with a linear flushed buffer" $
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
                                (zr, zs) <- runZiftTestWith state pchan sets zf
                                zr `shouldBe` ZiftSuccess 2
                                zs `shouldBe` ZiftState {bufferedOutput = []}
                                buffer <- readAllFrom pchan
                                buffer `shouldBe`
                                    reverse
                                        (bufferedOutput st2 ++
                                         bufferedOutput st1 ++
                                         bufferedOutput state)
        describe "Monad Zift" $
            describe ">>=" $
            it "succeeds with a flushed buffer after the first output" $
            forAll genUnchecked $ \sets ->
                forAll genUnchecked $ \state ->
                    forAll genUnchecked $ \zo1 ->
                        forAll genUnchecked $ \zo2 -> do
                            let zf1 =
                                    Zift $ \_ st ->
                                        pure
                                            ( ZiftSuccess (2 :: Int)
                                            , st
                                              { bufferedOutput =
                                                    zo1 : bufferedOutput st
                                              })
                            let zf2 n =
                                    Zift $ \_ st ->
                                        pure
                                            ( ZiftSuccess (n + 1)
                                            , st
                                              { bufferedOutput =
                                                    zo2 : bufferedOutput st
                                              })
                            let zf = zf1 >>= zf2
                            pchan <- atomically newTChan
                            (zr, zs) <- runZiftTestWith state pchan sets zf
                            zr `shouldBe` ZiftSuccess 3
                            zs `shouldBe` ZiftState {bufferedOutput = [zo2]}
                            buffer <- readAllFrom pchan
                            buffer `shouldBe`
                                (reverse (bufferedOutput state) ++ [zo1])
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
