{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Zifter
    ( ziftWith
    ) where

import Introduction

import Data.List (isInfixOf)

import qualified System.Directory as D
       (canonicalizePath, setPermissions, getPermissions,
        setOwnerExecutable)
import System.Environment (getProgName)
import qualified System.FilePath as FP (splitPath, joinPath)
import System.IO
       (hSetBuffering, BufferMode(LineBuffering), stderr, stdout)
import System.Process (shell, createProcess, waitForProcess)

import Zifter.Hindent
import Zifter.OptParse
import Zifter.PreProcess.Types
import Zifter.Types

ziftWith :: ZiftSetup -> IO ()
ziftWith setup = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    (d, Settings) <- getInstructions
    case d of
        DispatchRun -> run setup
        DispatchInstall -> install

run :: ZiftSetup -> IO ()
run ZiftSetup {..} = do
    rootdir <- autoRootDir
    r <- preprocess ziftPreprocessor rootdir
    case r of
        PreProcessorSuccess () -> pure ()
        PreProcessorFailed err -> die err

autoRootDir :: IO (Path Abs Dir)
autoRootDir = do
    pn <- getProgName
    here <- getCurrentDir
    (_, fs) <- listDir here
    unless (pn `elem` map (toFilePath . filename) fs) $
        die $
        unwords
            [ pn
            , "not found at"
            , toFilePath here
            , "the zift script must be run in the right directory."
            ]
    pure here

install :: IO ()
install = do
    rootdir <- autoRootDir
    let gitdir = rootdir </> dotGitDir
    gd <- doesDirExist gitdir
    let gitfile = rootdir </> dotGitFile
    gf <- doesFileExist gitfile
    print gitdir
    print gd
    print gitfile
    print gf
    ghd <-
        case (gd, gf) of
            (True, False) -> pure $ gitdir </> hooksDir
            (False, True) -> do
                contents <- readFile gitfile
                case splitAt (length "gitdir: ") contents of
                    ("gitdir: ", rest) -> do
                        case initMay rest of
                            Just gitdirref -> do
                                sp <-
                                    D.canonicalizePath $
                                    toFilePath rootdir ++ gitdirref
                                let figureOutDoubleDots =
                                        FP.joinPath . go [] . FP.splitPath
                                      where
                                        go acc [] = reverse acc
                                        go (a:acc) ("../":xs) = go acc xs
                                        go acc (x:xs) = go (x : acc) xs
                                realgitdir <-
                                    parseAbsDir $ figureOutDoubleDots sp
                                pure $ realgitdir </> hooksDir
    print ghd
    let preComitFile = ghd </> $(mkRelFile "pre-commit")
    writeFile preComitFile "./zift.hs run\n"
    pcf <- D.getPermissions (toFilePath preComitFile)
    D.setPermissions (toFilePath preComitFile) $ D.setOwnerExecutable True pcf

dotGitDir :: Path Rel Dir
dotGitDir = $(mkRelDir ".git")

dotGitFile :: Path Rel File
dotGitFile = $(mkRelFile ".git")

hooksDir :: Path Rel Dir
hooksDir = $(mkRelDir "hooks")
