module Zifter.Stack where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (isInfixOf)
import Path
import Path.IO
import System.Exit (ExitCode(..))
import System.IO
import System.Process

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Verbosity

import Zifter.Zift

stackBuildZift :: Zift ()
stackBuildZift = do
    () <- stackCheckAndPrintVersion
    stackBuild

stackCheckAndPrintVersion :: Zift ()
stackCheckAndPrintVersion = do
    let cmd = "stack --version"
    (_, mouth, _, ph) <-
        liftIO $ createProcess ((shell cmd) {std_out = CreatePipe})
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
    (_, fs) <- liftIO $ listDirRecur rd
    let cabalFiles =
            filter (not . isInfixOf ".stack-work" . toFilePath) $
            filter ((== ".cabal") . fileExtension) fs
    (concat <$>) $
        forM cabalFiles $ \cabalFile -> do
            pd <-
                liftIO $ readPackageDescription deafening $ toFilePath cabalFile
            let packageDesc = flattenPackageDescription pd
                name = unPackageName $ pkgName $ package packageDesc
                libname = name ++ ":lib"
                lib =
                    case library packageDesc of
                        Nothing -> []
                        Just _ -> [libname]
                testnames =
                    map (((name ++ ":test:") ++) . testName) $
                    testSuites packageDesc
            pure [(name, lib ++ testnames)]

stackBuild :: Zift ()
stackBuild = do
    rd <- getRootDir
    let stack :: String -> Zift ()
        stack args = do
            let buildCmd = unwords ["stack", args]
            bec <-
                liftIO $ do
                    (_, _, _, bph) <-
                        createProcess
                            ((shell buildCmd) {cwd = Just $ toFilePath rd})
                    waitForProcess bph
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
