{-# LANGUAGE TemplateHaskell #-}

module Zifter.Hindent where

import Introduction

import Data.List (isInfixOf)

import Control.Monad.Fail

import qualified System.Directory as D
       (canonicalizePath, setPermissions, getPermissions,
        setOwnerExecutable)
import System.Environment (getProgName)
import qualified System.FilePath as FP (splitPath, joinPath)
import System.Process (shell, createProcess, waitForProcess)

import Zifter.OptParse
import Zifter.PreProcess
import Zifter.Types

hindentPreProcessor :: PreProcessor ()
hindentPreProcessor = do
    () <- hindentCheckAndPrintVersion
    rootdir <- getRootDir
    fs <- liftIO $ snd <$> listDirRecur rootdir
    let sources = filter (not . hidden) $ filter ((== ".hs") . fileExtension) fs
    for_ sources hindentSingleSource

hindentCheckAndPrintVersion :: PreProcessor ()
hindentCheckAndPrintVersion = do
    let cmd = "hindent --version"
    ec <-
        liftIO $
        createProcess (shell cmd) >>= (waitForProcess . (\(_, _, _, ph) -> ph))
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()

hindentSingleSource :: Path Abs File -> PreProcessor ()
hindentSingleSource file =
    PreProcessor $ \_ -> do
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
        (_, _, _, ph) <- createProcess cp
        ec <- waitForProcess ph
        pure $
            case ec of
                ExitSuccess -> PreProcessorSuccess ()
                ExitFailure c ->
                    PreProcessorFailed $
                    unwords
                        [ "Hindent failed on file"
                        , toFilePath file
                        , "with exit code"
                        , show c
                        ]

hidden :: Path Abs t -> Bool
hidden = any ((Just '.' ==) . headMay) . FP.splitPath . toFilePath
