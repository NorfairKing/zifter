{-# LANGUAGE CPP #-}

module Zifter.Stack where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function (on)
import Data.List
       (groupBy, intersect, isInfixOf, isPrefixOf, isSuffixOf, sortOn)
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

data Pkg =
    Pkg String
        [Target]
    deriving (Show, Eq)

data Target
    = Lib String
    | Test String
    | Bench String
    deriving (Show, Eq)

stackGetPackages :: Zift [Pkg]
stackGetPackages =
    combinePkgs <$> stackGetPackageTargetTuplesAccordingToStackIDE <*>
    stackGetPackageTargetTuplesAccordingToCabalFiles

combinePkgs :: [Pkg] -> [Pkg] -> [Pkg]
combinePkgs ps1 ps2 = unTups $ intersect (toTups ps1) (toTups ps2)
  where
    toTups :: [Pkg] -> [(String, Target)]
    toTups = concatMap (\(Pkg p ts) -> map ((,) p) ts)
    unTups :: [(String, Target)] -> [Pkg]
    unTups =
        mapMaybe
            (\tups -> Pkg <$> (fst <$> headMay tups) <*> pure (map snd tups)) .
        groupBy ((==) `on` fst) . sortOn fst

stackGetPackageTargetTuplesAccordingToStackIDE :: Zift [Pkg]
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
    let isLib = isSuffixOf ":lib"
    let isTest = isInfixOf ":test:"
    let isBench = isInfixOf ":bench:"
    pure $
        flip map packages $ \p ->
            let relevantTargets = filter (isPrefixOf p) targets
            in Pkg p $
               map Lib (filter isLib relevantTargets) ++
               map Test (filter isTest relevantTargets) ++
               map Bench (filter isBench relevantTargets)

stackGetPackageTargetTuplesAccordingToCabalFiles :: Zift [Pkg]
stackGetPackageTargetTuplesAccordingToCabalFiles = do
    rd <- getRootDir
    (_, fs) <- liftIO $ listDirRecur rd
    let cabalFiles =
            filter (not . isInfixOf ".stack-work" . toFilePath) $
            filter (not . hidden) $ filter ((== ".cabal") . fileExtension) fs
    forM cabalFiles $ \cabalFile -> do
        pd <- liftIO $ readPackage deafening $ toFilePath cabalFile
        let packageDesc = flattenPackageDescription pd
            name = unPackageName $ pkgName $ package packageDesc
            libname = name ++ ":lib"
            lib =
                case library packageDesc of
                    Nothing -> []
                    Just _ -> [Lib libname]
            testnames =
                map (((name ++ ":test:") ++) . testComponentName) $
                testSuites packageDesc
            benchnames =
                map (((name ++ ":bench:") ++) . benchComponentName) $
                benchmarks packageDesc
        pure $ Pkg name $ lib ++ map Test testnames ++ map Bench benchnames
#if MIN_VERSION_Cabal(2,0,0)
readPackage :: Verbosity -> FilePath -> IO GenericPackageDescription
readPackage = readGenericPackageDescription
#else
readPackage = readPackageDescription
#endif

#if MIN_VERSION_Cabal(2,0,0)
testComponentName :: TestSuite -> String
testComponentName = unUnqualComponentName . testName
#else
testComponentName = testName
#endif

#if MIN_VERSION_Cabal(2,0,0)
benchComponentName :: Benchmark -> String
benchComponentName = unUnqualComponentName . benchmarkName
#else
benchComponentName = benchmarkName
#endif
stackBuild :: Zift ()
stackBuild = do
    tups <- stackGetPackages
    stack "build" -- To get the dependencies done first
    mapM_ bePedanticAboutPackage tups

stack :: String -> Zift ()
stack args = do
    rd <- getRootDir
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
        ExitSuccess -> printPreprocessingDone $ unwords [buildCmd, "succeeded."]

hidden :: Path Abs t -> Bool
hidden = any ((Just '.' ==) . headMay) . FP.splitPath . toFilePath

bePedanticAboutPackage :: Pkg -> Zift ()
bePedanticAboutPackage (Pkg package_ targets) = do
    stack $ unwords ["clean", package_]
    mapM_ bePedanticAboutTarget targets

bePedanticAboutTarget :: Target -> Zift ()
bePedanticAboutTarget (Lib target) = do
    stack $ unwords ["build", target, "--pedantic"]
    stack $ unwords ["build", target, "--pedantic", "--haddock"]
bePedanticAboutTarget (Test target) = do
    stack $ unwords ["build", target, "--pedantic"]
    stack $ unwords ["build", target, "--pedantic", "--haddock"]
    stack $
        unwords
            [ "build"
            , target
            , "--pedantic"
            , "--test"
            , "--test-arguments='--fail-fast --seed=42'"
            ]
bePedanticAboutTarget (Bench target) = do
    stack $ unwords ["build", target, "--pedantic"]
    stack $ unwords ["build", target, "--pedantic", "--haddock"]
    stack $ unwords ["build", target, "--pedantic", "--bench"]
