{-# LANGUAGE TemplateHaskell #-}

module Zifter.Stack where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.Exit (ExitCode(..))
import System.Process
       (system, shell, readCreateProcessWithExitCode)

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
    ((_, _, psstr), (_, _, tsstr)) <-
        (,) <$>
        liftIO (readCreateProcessWithExitCode (shell "stack ide packages") "") <*>
        liftIO (readCreateProcessWithExitCode (shell "stack ide targets") "")
    pure $ matchUpPackagesAndTargets (lines psstr) (lines tsstr)

matchUpPackagesAndTargets :: [String] -> [String] -> [(String, [String])]
matchUpPackagesAndTargets packages targets =
    flip map packages $ \package ->
        (package, filter ((package ++ ":") `isPrefixOf`) targets)

stackBuild :: Zift ()
stackBuild = do
    tups <- stackGetPackageTargetTuples
    forM_ tups $ \(package, targets) -> do
        let cleanCmd = "stack clean " ++ package
        cec <- liftIO $ system cleanCmd
        case cec of
            ExitFailure c ->
                fail $ unwords [cleanCmd, "failed with exit code", show c]
            ExitSuccess -> pure ()
        forM_ targets $ \target -> do
            let buildCmd = "stack build --pedantic " ++ target
            bec <- liftIO $ system buildCmd
            case bec of
                ExitFailure c ->
                    fail $ unwords [buildCmd, "failed with exit code", show c]
                ExitSuccess -> pure ()
