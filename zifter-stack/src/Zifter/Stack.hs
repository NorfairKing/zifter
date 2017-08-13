module Zifter.Stack where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import Path
import System.Exit (ExitCode(..))
import System.IO
import System.Process

import Zifter.Zift

stackBuildZift :: Zift ()
stackBuildZift = do
    () <- stackCheckAndPrintVersion
    stackBuild

stackCheckAndPrintVersion :: Zift ()
stackCheckAndPrintVersion = do
    rd <- getRootDir
    let cmd = "stack --version"
    (_, mouth, _, ph) <-
        liftIO $
        createProcess
            ((shell cmd) {cwd = Just $ toFilePath rd, std_out = CreatePipe})
    ec <- liftIO $ waitForProcess ph
    case mouth of
        Nothing -> pure ()
        Just outh -> liftIO (hGetContents outh) >>= printZift
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()

stackGetPackageTargetTuples :: Zift [(String, [String])]
stackGetPackageTargetTuples = do
    rd <- getRootDir
    let getErrFrom cmd = do
            (_, _, merrh, ph) <-
                liftIO $
                createProcess
                    ((shell cmd)
                     {cwd = Just $ toFilePath rd, std_err = CreatePipe})
            ec <- liftIO $ waitForProcess ph
            case ec of
                ExitFailure c ->
                    fail $ unwords [show cmd, "failed with exit code", show c]
                ExitSuccess -> pure ()
            case merrh of
                Nothing ->
                    fail $ unwords ["Failed to capture output of", show cmd]
                Just outh -> liftIO (hGetContents outh)
    outt <- getErrFrom "stack ide targets"
    outp <- getErrFrom "stack ide packages"
    let targets = lines outt
    let packages = lines outp
    pure $ flip map packages $ \t -> (t, filter (isPrefixOf t) targets)

stackBuild :: Zift ()
stackBuild = do
    rd <- getRootDir
    let stack :: String -> Zift ()
        stack args = do
            let buildCmd = unwords ["stack", args]
            (_, mouth, merrh, bph) <-
                liftIO $
                createProcess
                    ((shell buildCmd)
                     { cwd = Just $ toFilePath rd
                     , std_out = CreatePipe
                     , std_err = CreatePipe
                     })
            bec <- liftIO $ waitForProcess bph
            case mouth of
                Nothing -> pure ()
                Just outh -> liftIO (hGetContents outh) >>= printZift
            case merrh of
                Nothing -> pure ()
                Just errh -> liftIO (hGetContents errh) >>= printZift
            case bec of
                ExitFailure c ->
                    fail $ unwords [buildCmd, "failed with exit code", show c]
                ExitSuccess ->
                    printPreprocessingDone $ unwords [buildCmd, "succeeded."]
    tups <- stackGetPackageTargetTuples
    stack "build"
    forM_ tups $ \(package_, targets) -> do
        stack $ unwords ["clean", package_]
        forM_ targets $ \target ->
            stack $ unwords ["build --pedantic --haddock --test", target]
