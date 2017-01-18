{-# LANGUAGE TemplateHaskell #-}

module Zifter.Cabal where

import Control.Monad.IO.Class
import Data.Foldable
import Path
import Path.IO
import System.Exit (ExitCode(..))
import System.Process (system)

import Zifter.Zift

cabalFormatZift :: Zift ()
cabalFormatZift = do
    () <- cabalCheckAndPrintVersion
    cabalFormat

cabalCheckAndPrintVersion :: Zift ()
cabalCheckAndPrintVersion = do
    let cmd = "cabal --version"
    ec <- liftIO $ system cmd
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()

cabalFormat :: Zift ()
cabalFormat = do
    let formatCmd = "cabal format" -- TODO read the cabal file to find the target.
    rd <- getRootDir
    cabalFiles <-
        liftIO $
        (filter ((".cabal" ==) . fileExtension) . snd) <$> listDirRecur rd
    for_ cabalFiles $ \cabalFile -> do
        cec <- liftIO $ system $ unwords [formatCmd, toFilePath cabalFile]
        case cec of
            ExitFailure c ->
                fail $ unwords [formatCmd, "failed with exit code", show c]
            ExitSuccess -> pure ()
