{-# LANGUAGE TemplateHaskell #-}

module Zifter.GoogleJavaFormat where

import Control.Monad
import Data.Foldable

import System.Exit
import qualified System.FilePath as FP
import System.IO
import System.Process

import Safe

import Path
import Path.IO

import Zifter.Zift

googleJavaFormatZift :: Zift ()
googleJavaFormatZift = do
    format <- getJavaFormatter
    rd <- getRootDir
    fs <- liftIO $ snd <$> listDirRecur rd
    let sources =
            filter (not . hidden) $ filter ((== ".java") . fileExtension) fs
    for_ sources format

getJavaFormatter :: Zift (Path Abs File -> Zift ())
getJavaFormatter = do
    downloadJavaFormatIfMissing
    jarFile <- javaFormatterJarFile
    pure $ \abspath ->
        runZiftCommand $
        unwords
            [ "java"
            , "-jar"
            , toFilePath jarFile
            , "--replace"
            , toFilePath abspath
            ]

downloadJavaFormatIfMissing :: Zift ()
downloadJavaFormatIfMissing = do
    jarFile <- javaFormatterJarFile
    exists <- doesFileExist jarFile
    unless exists $ do
        ensureDir $ parent jarFile
        runZiftCommand $
            unwords
                [ wgetCmd
                , javaFormatterUrl
                , "--output-document"
                , toFilePath jarFile
                ]

javaFormatDir :: Zift (Path Abs Dir)
javaFormatDir = (</> $(mkRelDir "google-java-format")) <$> getTempDir

javaFormatterJarFile :: Zift (Path Abs File)
javaFormatterJarFile =
    (</> $(mkRelFile "google-java-format-1.0-all-deps.jar")) <$> javaFormatDir

wgetCmd :: String
wgetCmd = "wget"

javaFormatterUrl :: FilePath
javaFormatterUrl =
    "https://github.com/google/google-java-format/releases/download/google-java-format-1.0/google-java-format-1.0-all-deps.jar"

hidden :: Path Abs t -> Bool
hidden = any ((Just '.' ==) . headMay) . FP.splitPath . toFilePath

runZiftCommand :: String -> Zift ()
runZiftCommand command = do
    let cp = shell command
    (_, mouth, merrh, ph) <- liftIO $ createProcess cp
    ec <- liftIO $ waitForProcess ph
    case mouth of
        Nothing -> pure ()
        Just outh -> liftIO (hGetContents outh) >>= printZift
    case merrh of
        Nothing -> pure ()
        Just errh -> liftIO (hGetContents errh) >>= printPreprocessingError
    case ec of
        ExitFailure c ->
            fail $ unwords [command, "failed with exit code", show c]
        ExitSuccess -> printPreprocessingDone $ unwords [command, "succeeded."]
