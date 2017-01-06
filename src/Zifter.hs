{-# LANGUAGE TemplateHaskell #-}
module Zifter where

import Introduction

import Data.List (isInfixOf)

import System.Environment (getProgName)
import qualified System.FilePath as FP (splitPath,joinPath)
import qualified System.Directory as D (canonicalizePath, setPermissions, getPermissions, setOwnerExecutable)
import System.Process (shell, createProcess, waitForProcess)

import Zifter.Types
import Zifter.OptParse

zift :: IO ()
zift = do
    (d, Settings) <- getInstructions
    dispatch d

dispatch :: Dispatch -> IO ()
dispatch DispatchRun = run
dispatch DispatchInstall = install

run :: IO ()
run = do
    rootdir <- autoRootDir
    hindent rootdir

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

hindent :: Path Abs Dir -> IO ()
hindent rootdir = do
    (_, fs) <- listDirRecur rootdir
    let sources = filter (not . hidden) $ filter ((== ".hs") . fileExtension) fs
    ec <- createProcess (shell "hindent --version") >>= (waitForProcess . (\(_, _, _, ph) -> ph))
    case ec of
        ExitFailure _ -> die "Hindent was not found."
        ExitSuccess -> do
            phs <-
                forM sources $ \fp -> do
                    let cmd =
                            unwords ["hindent", "--indent-size", "4", "--line-length", "100", toFilePath fp]
                    let cp = shell cmd
                    (_, _, _, ph) <- createProcess cp
                    pure (cmd, ph)
            forM_ phs $ \(cmd, ph) -> do
                ec <- waitForProcess ph
                putStrLn cmd
                print ec

hidden :: Path Abs t -> Bool
hidden = any ((Just '.' ==) . headMay) . FP.splitPath . toFilePath

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
    ghd <- case (gd, gf) of
        (True, False) -> pure $ gitdir </> hooksDir
        (False, True) -> do
            contents <- readFile gitfile
            case splitAt (length "gitdir: ") contents of
                ("gitdir: ", rest) -> do
                    case initMay rest of
                        Just gitdirref -> do
                            sp <- D.canonicalizePath $ toFilePath rootdir ++ gitdirref
                            let figureOutDoubleDots = FP.joinPath . go [] . FP.splitPath
                                  where
                                    go acc [] = reverse acc
                                    go (a:acc) ("../":xs) = go acc xs
                                    go acc (x:xs) = go (x:acc) xs
                            realgitdir <- parseAbsDir $ figureOutDoubleDots sp
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
