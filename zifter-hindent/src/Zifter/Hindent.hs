{-# LANGUAGE TemplateHaskell #-}

module Zifter.Hindent where

import Control.Monad.IO.Class
import Data.Foldable
import Path
import Path.IO
import Safe
import System.Exit (ExitCode(..))
import qualified System.FilePath as FP (splitPath)
import System.Process (shell, createProcess, waitForProcess)

import Zifter.Zift

hindentZift :: Zift ()
hindentZift = do
    () <- hindentCheckAndPrintVersion
    rd <- getRootDir
    fs <- liftIO $ snd <$> listDirRecur rd
    let sources = filter (not . hidden) $ filter ((== ".hs") . fileExtension) fs
    for_ sources hindentSingleSource

hindentCheckAndPrintVersion :: Zift ()
hindentCheckAndPrintVersion = do
    let cmd = "hindent --version"
    ec <-
        liftIO $
        createProcess (shell cmd) >>= (waitForProcess . (\(_, _, _, ph) -> ph))
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()

hindentSingleSource :: Path Abs File -> Zift ()
hindentSingleSource file = do
    let cmd =
            unwords
                [ "hindent"
                , "--indent-size"
                , "4"
                , "--line-length"
                , "80"
                , toFilePath file
                ]
    let cp = shell cmd
    ec <-
        liftIO $ do
            (_, _, _, ph) <- createProcess cp
            waitForProcess ph
    case ec of
        ExitSuccess ->
            printPreprocessingDone $
            unwords
                ["Formatted Haskell source file with hindent:", toFilePath file]
        ExitFailure c -> do
            printPreprocessingError $
                unwords
                    ["Failed to format Haskell source file:", toFilePath file]
            fail $ unwords [cmd, "failed", "with exit code", show c]

hidden :: Path Abs t -> Bool
hidden = any ((Just '.' ==) . headMay) . FP.splitPath . toFilePath
