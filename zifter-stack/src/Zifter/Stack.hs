module Zifter.Stack where

import Control.Monad
import Control.Monad.IO.Class
import Path
import Path.IO
import System.Exit (ExitCode(..))
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
    ec <- liftIO $ system cmd
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()

stackGetPackageTargetTuples :: Zift [(String, [String])]
stackGetPackageTargetTuples = do
    rd <- getRootDir
    (_, fs) <- liftIO $ listDirRecur rd
    let cabalFiles = filter ((== ".cabal") . fileExtension) fs
    (concat <$>) $
        forM cabalFiles $ \cabalFile -> do
            pd <-
                liftIO $ readPackageDescription deafening $ toFilePath cabalFile
            let packageDesc = flattenPackageDescription pd
                name = unPackageName $ pkgName $ package packageDesc
                libname = name ++ ":lib"
                testnames =
                    map (((name ++ ":test:") ++) . testName) $
                    testSuites packageDesc
            pure [(name, libname : testnames)]

stackBuild :: Zift ()
stackBuild = do
    rd <- getRootDir
    tups <- stackGetPackageTargetTuples
    forM_ tups $ \(package_, targets) -> do
        let cleanCmd = "stack clean " ++ package_
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
            let buildCmd = "stack build --pedantic --haddock --test " ++ target
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
