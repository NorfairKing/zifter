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
import Control.Monad
import Data.Foldable
import Data.GenValidity.Path ()
import qualified Data.Map as M

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
    describe "advanceBookkeeperState" $ do
        it
            "keep strack of any first message as an incomplete buffer, and does not stop or output" $ do
            forAll genUnchecked $ \rp ->
                forAll genUnchecked $ \zo ->
                    advanceBookkeeperState
                        initialBookkeeperState
                        (ZiftOutputMessage rp zo) `shouldBe`
                    Just
                        ( BookkeeperState
                              (M.fromList
                                   [(rp, Incomplete $ singletonBuffer zo)])
                        , mempty)
        let runOn
                :: BookkeeperState
                -> [ZiftOutputMessage]
                -> Maybe (BookkeeperState, OutputBuffer)
            runOn start = foldl' go (Just (start, mempty))
              where
                go
                    :: Maybe (BookkeeperState, OutputBuffer)
                    -> ZiftOutputMessage
                    -> Maybe (BookkeeperState, OutputBuffer)
                go mst m = do
                    (st, b) <- mst
                    (st', b') <- advanceBookkeeperState st m
                    pure (st', b `mappend` b')
        it
            "keep strack of any non-zero number of messages on the same path as an incomplete buffer, and does not stop or output" $ do
            forAll genUnchecked $ \rp ->
                forAll (genUnchecked `suchThat` (not . null)) $ \zos ->
                    runOn
                        initialBookkeeperState
                        (map (ZiftOutputMessage rp) zos) `shouldBe`
                    Just
                        ( BookkeeperState
                              (M.fromList [(rp, Incomplete $ bufferOf zos)])
                        , mempty)
        it
            "flushes any output on a path with only L's immediately on completion." $ do
            forAll (RecursionPath <$> (genListOf $ pure L)) $ \rp ->
                forAll genUnchecked $ \mob ->
                    forAll genUnchecked $ \(BookkeeperState bs') ->
                        let (bs, obs) =
                                case mob of
                                    Nothing ->
                                        ( BookkeeperState $ M.delete rp bs'
                                        , mempty -- Either there was no record yet
                                         )
                                    Just obs ->
                                        ( BookkeeperState $
                                          M.insert rp (Incomplete obs) bs'
                                        , obs -- Or there was, but it was incomplete
                                         )
                            mr =
                                advanceBookkeeperState bs (CompletionMessage rp)
                        in case mr of
                               Nothing ->
                                   expectationFailure "Should not halt yet."
                               Just (BookkeeperState bs, os) -> do
                                   os `shouldBe` obs
                                   M.lookup rp bs `shouldBe` Just Flushed
    describe "shouldFlush" $ do
        it "is always true for all (== L) paths" $ do
            forAll genUnchecked $ \bs ->
                forAll (genListOf $ pure L) $ \ls ->
                    let rp = RecursionPath ls
                    in shouldFlush bs rp `shouldBe` True
        it "is always true for the empty recursion path (the last one)" $ do
            forAll genUnchecked $ \bs ->
                shouldFlush bs (RecursionPath []) `shouldBe` True
        it
            "is not true in this simple case for the right-hand side of a single split" $ do
            forAll genUnchecked $ \ob ->
                shouldFlush
                    (BookkeeperState $
                     M.fromList [(RecursionPath [L], Incomplete ob)])
                    (RecursionPath [R]) `shouldBe`
                False
        it
            "is not true in this simple case for the left-hand side of second split" $ do
            forAll genUnchecked $ \ob1 ->
                forAll genUnchecked $ \ob2 ->
                    shouldFlush
                        (BookkeeperState $
                         M.fromList
                             [ (RecursionPath [L, L], Incomplete ob1)
                             , (RecursionPath [L, R], Incomplete ob2)
                             ])
                        (RecursionPath [R, L]) `shouldBe`
                    False
        -- let isComplete :: OutputRecord -> Bool
        --     isComplete (Complete _) = True
        --     isComplete Flushed = True
        --     isComplete _ = False
        -- it "Leaves a record completed if it gets a message after completion" $
        --     forAll genUnchecked $ \bs ->
        --         forAll genUnchecked $ \rp ->
        --             forAll genUnchecked $ \zo ->
        --                 let mres =
        --                         foldM
        --                             advanceBookkeeperState
        --                             bs
        --                             [ CompletionMessage rp
        --                             , ZiftOutputMessage rp zo
        --                             ]
        --                 in case mres of
        --                        Nothing ->
        --                            expectationFailure "should not have halted."
        --                        Just (BookkeeperState res) ->
        --                            isComplete <$>
        --                            M.lookup rp res `shouldBe` Just True
        -- it "completes an output record when a completion message is sent" $
        --     forAll genUnchecked $ \bs ->
        --         forAll genUnchecked $ \rp ->
        --             let mres = advanceBookkeeperState bs (CompletionMessage rp)
        --             in case mres of
        --                    Nothing ->
        --                        expectationFailure "should not have halted."
        --                    Just (BookkeeperState res) ->
        --                        isComplete <$>
        --                        M.lookup rp res `shouldBe` Just True
        -- it "leaves a path completed if it was completed already but not flushed" $
        --     forAll genUnchecked $ \bs ->
        --         forAll genUnchecked $ \rp ->
        --             let mres =
        --                     foldM
        --                         advanceBookkeeperState
        --                         bs
        --                         [CompletionMessage rp, CompletionMessage rp]
        --             in case mres of
        --                    Nothing ->
        --                        expectationFailure "should not have halted."
        --                    Just (BookkeeperState res) ->
        --                        isComplete <$>
        --                        M.lookup rp res `shouldBe` Just True
        -- it "leaves a path flushed if any new messages arrive after flushing" $
        --     True
        -- monadSpec @ZiftResult -- It's not a real monad, but close enough.
    -- describe "Zift" $ do
    --     describe "Monoid Zift" $ do
    --         describe "mempty" $
    --             it "succeeds with empty buffer" $
    --             forAll genUnchecked $ \sets -> do
    --                 let zf = mempty :: Zift ()
    --                 zr <- runZiftTest sets zf
    --                 zr `shouldBe` ZiftSuccess ()
    --         describe "mappend" $
    --             it
    --                 "succeeds with empty buffers but with the output in the channel" $
    --             forAll genUnchecked $ \sets ->
    --                 forAll genUnchecked $ \zo1 ->
    --                     forAll genUnchecked $ \zo2 -> do
    --                         let zf1 =
    --                                 Zift $ \_ _ ->
    --                                     pure
    --                                         ( ZiftSuccess ())
    --                         let zf2 =
    --                                 Zift $ \_ _ ->
    --                                     pure
    --                                         ( ZiftSuccess ()
    --                                         , ZiftState {bufferedOutput = [zo2]})
    --                         let zf = zf1 `mappend` zf2
    --                         pchan <- atomically newTChan
    --                         (zr, zs) <- runZiftTestWithChan pchan sets zf
    --                         zr `shouldBe` ZiftSuccess ()
    --                         zs `shouldBe` ZiftState {bufferedOutput = []}
    --                         atomically (readTChan pchan) `shouldReturn` zo1
    --                         atomically (readTChan pchan) `shouldReturn` zo2
    --     describe "Functor Zift" $
    --         describe "fmap" $
    --         it "it succeeds with a flushed buffer" $
    --         forAll genUnchecked $ \sets ->
    --             forAll genUnchecked $ \state -> do
    --                 let zf =
    --                         fmap (+ 1) $
    --                         Zift $ \_ st -> pure (ZiftSuccess (0 :: Int), st)
    --                 pchan <- atomically newTChan
    --                 (zr, zs) <- runZiftTestWith state pchan sets zf
    --                 zr `shouldBe` ZiftSuccess 1
    --                 zs `shouldBe` ZiftState {bufferedOutput = []}
    --                 buffer <- readAllFrom pchan
    --                 buffer `shouldBe` reverse (bufferedOutput state)
    --     describe "Applicative Zift" $ do
    --         describe "pure" $
    --             it "succeeds with the same state" $
    --             forAll genUnchecked $ \sets ->
    --                 forAll genUnchecked $ \state -> do
    --                     let zf = pure () :: Zift ()
    --                     (zr, zs) <- runZiftTestWithState state sets zf
    --                     zr `shouldBe` ZiftSuccess ()
    --                     zs `shouldBe` state
    --         describe "<*>" $ do
    --             it
    --                 "succeeds with a linear flushed buffer if used at the top-level" $
    --                 forAll genUnchecked $ \sets ->
    --                     forAll genUnchecked $ \state ->
    --                         forAll genUnchecked $ \st1 ->
    --                             forAll genUnchecked $ \st2 -> do
    --                                 let zf1 =
    --                                         Zift $ \_ st ->
    --                                             pure
    --                                                 ( ZiftSuccess (+ 1)
    --                                                 , st `mappend` st1)
    --                                 let zf2 =
    --                                         Zift $ \_ st ->
    --                                             pure
    --                                                 ( ZiftSuccess (1 :: Int)
    --                                                 , st `mappend` st2)
    --                                 let zf = zf1 <*> zf2
    --                                 pchan <- atomically newTChan
    --                                 (zr, zs) <-
    --                                     runZiftTestWith state pchan sets zf
    --                                 zr `shouldBe` ZiftSuccess 2
    --                                 zs `shouldBe`
    --                                     ZiftState {bufferedOutput = []}
    --                                 buffer <- readAllFrom pchan
    --                                 buffer `shouldBe`
    --                                     reverse
    --                                         (bufferedOutput st2 ++
    --                                          bufferedOutput st1 ++
    --                                          bufferedOutput state)
    --             it "fails immediately if the first of the two actions failed" $
    --                 forAll genUnchecked $ \sets ->
    --                     forAll genUnchecked $ \state ->
    --                         forAll genUnchecked $ \st1 ->
    --                             forAll genUnchecked $ \failedMessage -> do
    --                                 let zf1 =
    --                                         Zift $ \_ st ->
    --                                             pure
    --                                                 ( ZiftFailed failedMessage :: ZiftResult (Int -> String)
    --                                                 , st `mappend` st1)
    --                                 let zf2 = do
    --                                         liftIO $
    --                                             threadDelay $ 5 * 1000 * 1000
    --                                         pure 1 :: Zift Int
    --                                 let zf = zf1 <*> zf2
    --                                 pchan <- atomically newTChan
    --                                 (zr, zs) <-
    --                                     runZiftTestWith state pchan sets zf
    --                                 zr `shouldBe`
    --                                     (ZiftFailed failedMessage :: ZiftResult String)
    --                                 zs `shouldBe`
    --                                     ZiftState
    --                                     { bufferedOutput =
    --                                           bufferedOutput st1 ++
    --                                           bufferedOutput state
    --                                     }
    --                                 buffer <- readAllFrom pchan
    --                                 buffer `shouldBe` []
    --             it "fails immediately if the second of the two actions failed" $
    --                 forAll genUnchecked $ \sets ->
    --                     forAll genUnchecked $ \state ->
    --                         forAll genUnchecked $ \st2 ->
    --                             forAll genUnchecked $ \failedMessage -> do
    --                                 let zf1 = do
    --                                         liftIO $
    --                                             threadDelay $ 5 * 1000 * 1000
    --                                         pure show :: Zift (Int -> String)
    --                                 let zf2 =
    --                                         Zift $ \_ st ->
    --                                             pure
    --                                                 ( ZiftFailed failedMessage :: ZiftResult Int
    --                                                 , st `mappend` st2)
    --                                 let zf = zf1 <*> zf2
    --                                 pchan <- atomically newTChan
    --                                 (zr, zs) <-
    --                                     runZiftTestWith state pchan sets zf
    --                                 zr `shouldBe`
    --                                     (ZiftFailed failedMessage :: ZiftResult String)
    --                                 zs `shouldBe`
    --                                     ZiftState
    --                                     { bufferedOutput =
    --                                           bufferedOutput st2 ++
    --                                           bufferedOutput state
    --                                     }
    --                                 buffer <- readAllFrom pchan
    --                                 buffer `shouldBe` []
    --             it
    --                 "Orders the messages correctly when they are printed in a for_ loop" $
    --                 forAll genUnchecked $ \ls ->
    --                     forAll genUnchecked $ \rl ->
    --                         forAll genUnchecked $ \sets -> do
    --                             let zf =
    --                                     withRecursionList rl $
    --                                     for_ ls addZiftOutput
    --                             pchan <- atomically newTChan
    --                             (zr, zs) <- runZiftTestWithChan pchan sets zf
    --                             zr `shouldBe` ZiftSuccess ()
    --                             buffer <- readAllFrom pchan
    --                             (buffer ++ reverse (bufferedOutput zs)) `shouldBe`
    --                                 ls
    --     describe "Monad Zift" $
    --         describe ">>=" $ do
    --             it "succeeds with a flushed buffer after the first output" $
    --                 forAll genUnchecked $ \sets ->
    --                     forAll genUnchecked $ \state ->
    --                         forAll genUnchecked $ \st1 ->
    --                             forAll genUnchecked $ \st2 -> do
    --                                 let zf1 =
    --                                         Zift $ \_ st ->
    --                                             pure
    --                                                 ( ZiftSuccess (2 :: Int)
    --                                                 , st `mappend` st1)
    --                                 let zf2 n =
    --                                         Zift $ \_ st ->
    --                                             pure
    --                                                 ( ZiftSuccess (n + 1)
    --                                                 , st `mappend` st2)
    --                                 let zf = zf1 >>= zf2
    --                                 pchan <- atomically newTChan
    --                                 (zr, zs) <-
    --                                     runZiftTestWith state pchan sets zf
    --                                 zr `shouldBe` ZiftSuccess 3
    --                                 zs `shouldBe` st2
    --                                 buffer <- readAllFrom pchan
    --                                 buffer `shouldBe`
    --                                     reverse
    --                                         (bufferedOutput st1 ++
    --                                          bufferedOutput state)
    --             it "fails if the first part failed" $
    --                 forAll genUnchecked $ \sets ->
    --                     forAll genUnchecked $ \state ->
    --                         forAll genUnchecked $ \st1 ->
    --                             forAll genUnchecked $ \st2 ->
    --                                 forAll genUnchecked $ \failMsg -> do
    --                                     let zf1 =
    --                                             Zift $ \_ st ->
    --                                                 pure
    --                                                     ( ZiftFailed failMsg :: ZiftResult Int
    --                                                     , st `mappend` st1)
    --                                     let zf2 n =
    --                                             Zift $ \_ st ->
    --                                                 pure
    --                                                     ( ZiftSuccess (n + 1)
    --                                                     , st `mappend` st2)
    --                                     let zf = zf1 >>= zf2
    --                                     pchan <- atomically newTChan
    --                                     (zr, zs) <-
    --                                         runZiftTestWith state pchan sets zf
    --                                     zr `shouldBe` ZiftFailed failMsg
    --                                     zs `shouldBe` mempty
    --                                     buffer <- readAllFrom pchan
    --                                     buffer `shouldBe`
    --                                         reverse
    --                                             (bufferedOutput st1 ++
    --                                              bufferedOutput state)
    --             it "fails if the second part failed" $
    --                 forAll genUnchecked $ \sets ->
    --                     forAll genUnchecked $ \state ->
    --                         forAll genUnchecked $ \st1 ->
    --                             forAll genUnchecked $ \st2 ->
    --                                 forAll genUnchecked $ \failMsg -> do
    --                                     let zf1 =
    --                                             Zift $ \_ st ->
    --                                                 pure
    --                                                     ( ZiftSuccess (1 :: Int)
    --                                                     , st `mappend` st1)
    --                                     let zf2 _ =
    --                                             Zift $ \_ st ->
    --                                                 pure
    --                                                     ( ZiftFailed failMsg :: ZiftResult (Int -> Int)
    --                                                     , st `mappend` st2)
    --                                     let zf = zf1 >>= zf2
    --                                     pchan <- atomically newTChan
    --                                     (zr, zs) <-
    --                                         runZiftTestWith state pchan sets zf
    --                                     case zr of
    --                                         ZiftSuccess _ ->
    --                                             expectationFailure
    --                                                 "should have failed."
    --                                         ZiftFailed msg -> do
    --                                             msg `shouldBe` failMsg
    --                                             zs `shouldBe` st2
    --                                             buffer <- readAllFrom pchan
    --                                             buffer `shouldBe`
    --                                                 reverse
    --                                                     (bufferedOutput st1 ++
    --                                                      bufferedOutput state)
    --             it
    --                 "Orders the messages correctly when they are printed in a forM_ loop at any depth" $
    --                 forAll genUnchecked $ \ls ->
    --                     forAll genUnchecked $ \rl ->
    --                         forAll genUnchecked $ \sets -> do
    --                             let zf = forM_ ls addZiftOutput
    --                                 zf' = withRecursionList rl zf
    --                             pchan <- atomically newTChan
    --                             (zr, zs) <- runZiftTestWithChan pchan sets zf'
    --                             zr `shouldBe` ZiftSuccess ()
    --                             buffer <- readAllFrom pchan
    --                             (buffer ++ reverse (bufferedOutput zs)) `shouldBe`
    --                                 ls
    --             it "Orders the messages in this do-notation correctly" $
    --                 forAll genUnchecked $ \(m1, m2, m3) ->
    --                     forAll genUnchecked $ \rl ->
    --                         forAll genUnchecked $ \sets -> do
    --                             let zf =
    --                                     withRecursionList rl $ do
    --                                         addZiftOutput m1
    --                                         addZiftOutput m2
    --                                         addZiftOutput m3
    --                             pchan <- atomically newTChan
    --                             (zr, zs) <- runZiftTestWithChan pchan sets zf
    --                             zr `shouldBe` ZiftSuccess ()
    --                             buffer <- readAllFrom pchan
    --                             (buffer ++ reverse (bufferedOutput zs)) `shouldBe`
    --                                 [m1, m2, m3]
    --     describe "MonadFail Zift" $ do
    --         describe "fail" $
    --             it "just results in a ZiftFailed" $
    --             forAll genUnchecked $ \sets ->
    --                 forAll genUnchecked $ \state -> do
    --                     let s = "Test"
    --                     let zf = fail s :: Zift ()
    --                     (zr, zs) <- runZiftTestWithState state sets zf
    --                     zr `shouldBe` ZiftFailed s
    --                     zs `shouldBe` state
    --         describe "liftIO . fail" $
    --             it "just returns ZiftFailed" $
    --             forAll genUnchecked $ \sets ->
    --                 forAll genUnchecked $ \state -> do
    --                     let s = "Test"
    --                     let zf = liftIO $ fail s :: Zift ()
    --                     (zr, zs) <- runZiftTestWithState state sets zf
    --                     zr `shouldBe` ZiftFailed ("user error (" ++ s ++ ")")
    --                     zs `shouldBe` state
    --     describe "tryFlushZiftBuffer" $ do
    --         it "does not do anything if there recursion list is not flushable" $
    --             forAll genUnchecked $ \state ->
    --                 forAllCtx $ \ctx ->
    --                     if flushable $ recursionList ctx
    --                         then pure () -- Not testing this part now.
    --                         else do
    --                             state' <- tryFlushZiftBuffer ctx state
    --                             state' `shouldBe` state
    --         it
    --             "flushes the entire buffer in the correct order, if the recursion list is empty" $
    --             forAll genUnchecked $ \state ->
    --                 forAllCtx $ \ctx ->
    --                     if flushable $ recursionList ctx
    --                         then do
    --                             state' <- tryFlushZiftBuffer ctx state
    --                             state' `shouldBe` state {bufferedOutput = []}
    --                             res <- readAllFrom $ printChan ctx
    --                             res `shouldBe` reverse (bufferedOutput state)
    --                         else pure ()

-- forAllCtx
--     :: Testable (IO b)
--     => (ZiftContext -> IO b) -> Property
-- forAllCtx func =
--     forAll genUnchecked $ \rd ->
--         forAll genUnchecked $ \sets ->
--             forAll genUnchecked $ \rl ->
--                 forAll genUnchecked $ \td -> do
--                     pchan <- atomically newTChan
--                     let zc =
--                             ZiftContext
--                             { rootdir = rd
--                             , tmpdir = td
--                             , settings = sets
--                             , printChan = pchan
--                             , recursionList = rl
--                             }
--                     func zc
--
-- runZiftTest :: Settings -> Zift a -> IO (ZiftResult a, ZiftState)
-- runZiftTest sets func = do
--     pchan <- atomically newTChan
--     runZiftTestWithChan pchan sets func
--
-- runZiftTestWithChan :: TChan ZiftOutput
--                     -> Settings
--                     -> Zift a
--                     -> IO (ZiftResult a, ZiftState)
-- runZiftTestWithChan = runZiftTestWith ZiftState {bufferedOutput = []}
--
-- runZiftTestWithState :: ZiftState
--                      -> Settings
--                      -> Zift a
--                      -> IO (ZiftResult a, ZiftState)
-- runZiftTestWithState state sets func = do
--     pchan <- atomically newTChan
--     runZiftTestWith state pchan sets func
--
-- runZiftTestWith
--     :: ZiftState
--     -> TChan ZiftOutput
--     -> Settings
--     -> Zift a
--     -> IO (ZiftResult a, ZiftState)
-- runZiftTestWith zs pchan sets func = do
--     rd <- getCurrentDir
--     td <- resolveDir rd ".zifter"
--     let zc =
--             ZiftContext
--             { rootdir = rd
--             , tmpdir = td
--             , settings = sets
--             , printChan = pchan
--             , recursionList = []
--             }
--     zift func zc zs
readAllFrom :: TChan a -> IO [a]
readAllFrom chan = do
    mr <- atomically $ tryReadTChan chan
    case mr of
        Nothing -> pure []
        Just r -> do
            rest <- readAllFrom chan
            pure (r : rest)
-- withRecursionList :: [LR] -> Zift a -> Zift a
-- withRecursionList rl zf =
--     zf {zift = \zc zs -> zift zf (zc {recursionList = rl}) zs}
