module Zifter.Cabal where

import Control.Monad.IO.Class
import Path
import Path.IO
import System.Exit (ExitCode(..))
import System.IO (hGetContents)
import System.Process

import Zifter.Zift

cabalFormatZift :: Zift ()
cabalFormatZift = do
    () <- cabalCheckAndPrintVersion
    cabalFormat

cabalCheckAndPrintVersion :: Zift ()
cabalCheckAndPrintVersion = do
    let cmd = "cabal --version"
    (_, mouth, _, ph) <-
        liftIO $ createProcess ((shell cmd) {std_out = CreatePipe})
    ec <- liftIO $ waitForProcess ph
    case mouth of
        Nothing -> pure ()
        Just outh -> liftIO (hGetContents outh) >>= printZift
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()

cabalFormat :: Zift ()
cabalFormat = do
    rd <- getRootDir
    cabalFiles <-
        liftIO $
        (filter ((".cabal" ==) . fileExtension) . snd) <$> listDirRecur rd
    forZ_ cabalFiles formatSingleCabalFile

formatSingleCabalFile :: Path Abs File -> Zift ()
formatSingleCabalFile cabalFile = do
    let formatCmd = "cabal format"
    cec <- liftIO $ system $ unwords [formatCmd, toFilePath cabalFile]
    case cec of
        ExitFailure c -> do
            printPreprocessingError $
                unwords ["Failed to format cabal file:", toFilePath cabalFile]
            fail $ unwords [formatCmd, "failed with exit code", show c]
        ExitSuccess ->
            printPreprocessingDone $
            unwords ["Formatted cabal file:", toFilePath cabalFile]
