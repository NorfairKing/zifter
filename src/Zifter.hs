module Zifter where

import Introduction

import Data.List (isInfixOf)

import System.Environment (getProgName)
import System.FilePath (splitPath)
import System.Process (shell, createProcess, waitForProcess)

import Zifter.Types

zift :: IO ()
zift = do
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
hidden = any ((Just '.' ==) . headMay) . splitPath . toFilePath
