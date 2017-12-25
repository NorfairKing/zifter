{-# LANGUAGE CPP #-}
module Zifter.Stack where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function (on)
import Data.List
       (groupBy, intersect, isInfixOf, isPrefixOf, sortOn)
import Data.Maybe (mapMaybe)
import Path
import Path.IO
import Safe
import System.Exit (ExitCode(..))
import qualified System.FilePath as FP (splitPath)
import System.IO
import System.Process

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.UnqualComponentName
#endif

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
    sps <- stackGetPackageTargetTuplesAccordingToStackIDE
    cps <- stackGetPackageTargetTuplesAccordingToCabalFiles
    pure $ combine $ uncombine sps `intersect` uncombine cps

combine :: Ord a => [(a, b)] -> [(a, [b])]
combine =
    mapMaybe (\tups -> (,) <$> (fst <$> headMay tups) <*> pure (map snd tups)) .
    groupBy ((==) `on` fst) . sortOn fst

uncombine :: Ord a => [(a, [b])] -> [(a, b)]
uncombine xs = do
    (a, bs) <- xs
    b <- bs
    pure (a, b)

stackGetPackageTargetTuplesAccordingToStackIDE :: Zift [(String, [String])]
stackGetPackageTargetTuplesAccordingToStackIDE = do
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

stackGetPackageTargetTuplesAccordingToCabalFiles :: Zift [(String, [String])]
stackGetPackageTargetTuplesAccordingToCabalFiles = do
    rd <- getRootDir
    (_, fs) <- liftIO $ listDirRecur rd
    let cabalFiles =
            filter (not . isInfixOf ".stack-work" . toFilePath) $
            filter (not . hidden) $ filter ((== ".cabal") . fileExtension) fs
    (concat <$>) $
        forM cabalFiles $ \cabalFile -> do
            pd <-
                liftIO $ readPackage
                    deafening $ toFilePath cabalFile
            let packageDesc = flattenPackageDescription pd
                name = unPackageName $ pkgName $ package packageDesc
                libname = name ++ ":lib"
                lib =
                    case library packageDesc of
                        Nothing -> []
                        Just _ -> [libname]
                testnames =
                    map (((name ++ ":test:") ++) .  testComponentName
                        ) $
                    testSuites packageDesc
            pure [(name, lib ++ testnames)]
  where
    readPackage =
#if MIN_VERSION_Cabal(2,0,0)
                    readGenericPackageDescription
#else
                    readPackageDescription
#endif
    testComponentName =
#if MIN_VERSION_Cabal(2,0,0)
                            unUnqualComponentName . testName
#else
                            testName
#endif

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
        forM_ targets $ \target -> do
            stack $ unwords ["build", target, "--pedantic"]
            stack $ unwords ["build", target, "--pedantic", "--haddock"]
            stack $ unwords
                [ "build"
                , target
                , "--pedantic"
                , "--test"
                , "--test-arguments='--fail-fast --seed=42'"
                ]

hidden :: Path Abs t -> Bool
hidden = any ((Just '.' ==) . headMay) . FP.splitPath . toFilePath
