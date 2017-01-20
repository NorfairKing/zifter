{-# LANGUAGE TemplateHaskell #-}

module Zifter.Stack where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Path
import System.Exit (ExitCode(..))
import System.Process

import Zifter.Zift

stackBuildZift :: Zift ()
stackBuildZift = do
    () <- stackCheckAndPrintVersion
    stackBuild

stackCheckAndPrintVersion :: Zift ()
stackCheckAndPrintVersion = do
    let cmd = "stack --version"
    ec <- liftIO $ system cmd
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()

stackGetPackageTargetTuples :: Zift [(String, [String])]
stackGetPackageTargetTuples = do
    rd <- getRootDir
    ((_, _, psstr), (_, _, tsstr)) <-
        (,) <$>
        liftIO
            (readCreateProcessWithExitCode
                 ((shell "stack ide packages") {cwd = Just $ toFilePath rd})
                 "") <*>
        liftIO
            (readCreateProcessWithExitCode
                 ((shell "stack ide targets") {cwd = Just $ toFilePath rd})
                 "")
    pure $ matchUpPackagesAndTargets (lines psstr) (lines tsstr)

matchUpPackagesAndTargets :: [String] -> [String] -> [(String, [String])]
matchUpPackagesAndTargets packages targets =
    flip map packages $ \package ->
        (package, filter ((package ++ ":") `isPrefixOf`) targets)

stackBuild :: Zift ()
stackBuild = do
    rd <- getRootDir
    tups <- stackGetPackageTargetTuples
    forM_ tups $ \(package, targets) -> do
        let cleanCmd = "stack clean " ++ package
        cec <-
            liftIO $ do
                (_, _, _, ph) <-
                    createProcess
                        ((shell cleanCmd) {cwd = Just $ toFilePath rd})
                waitForProcess ph
        case cec of
            ExitFailure c -> do
                printPreprocessingError $ unwords [cleanCmd, "failed."]
                fail $ unwords [cleanCmd, "failed with exit code", show c]
            ExitSuccess -> pure ()
        forM_ targets $ \target -> do
            let buildCmd = "stack build --pedantic " ++ target
            bec <-
                liftIO $ do
                    (_, _, _, bph) <-
                        createProcess
                            ((shell buildCmd) {cwd = Just $ toFilePath rd})
                    waitForProcess bph
            case bec of
                ExitFailure c -> do
                    printPreprocessingError $ unwords [buildCmd, "failed."]
                    fail $ unwords [buildCmd, "failed with exit code", show c]
                ExitSuccess ->
                    printPreprocessingDone $ unwords [buildCmd, "succeeded."]
