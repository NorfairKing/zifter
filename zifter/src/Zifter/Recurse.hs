{-# LANGUAGE TemplateHaskell #-}

module Zifter.Recurse
    ( recursiveZift
    , recursively
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Path
import Path.IO
import System.Exit
import System.Process

import Zifter.Script
import Zifter.Zift

recursiveZift :: ZiftScript ()
recursiveZift = do
    preprocessor $ do
        rd <- getRootDir
        printZiftMessage $
            unwords ["RECURSIVE PREPROCESSING STARTING FROM", toFilePath rd]
        recursively $ \ziftFile -> runZiftScript ziftFile "preprocess"
        printZiftMessage $
            unwords ["RECURSIVE PREPROCESSING FROM", toFilePath rd, "DONE."]
    checker $ do
        rd <- getRootDir
        printZiftMessage $
            unwords ["RECURSIVE CHECKING STARTING FROM", toFilePath rd]
        recursively $ \ziftFile -> runZiftScript ziftFile "check"
        printZiftMessage $
            unwords ["RECURSIVE CHECKING FROM", toFilePath rd, "DONE"]

recursively :: (Path Abs File -> Zift ()) -> Zift ()
recursively func = do
    fs <- findZiftFilesRecursively
    -- Do it in serial (for errors to show up nicely)
    -- TODO make it possible to run them in parallel instead?
    --      we might have to make it possible for zift to output something machine-readible instead.
    forM_ fs func

runZiftScript :: Path Abs File -> String -> Zift ()
runZiftScript scriptPath command = do
    rd <- getRootDir
    printZiftMessage $
        unwords
            [ "ZIFTING"
            , toFilePath scriptPath
            , "AS PART OF RECURSIVE ZIFT FROM"
            , toFilePath rd
            ]
    let cmd = unwords [toFilePath scriptPath, command]
    let cp = (shell cmd) {cwd = Just $ toFilePath $ parent scriptPath}
    ec <-
        liftIO $ do
            (_, _, _, ph) <- createProcess cp
            waitForProcess ph
    case ec of
        ExitSuccess -> pure ()
        ExitFailure c -> do
            printPreprocessingError "RECURSIVE ZIFT FAILED"
            fail $
                unwords
                    [ show cmd
                    , "failed with exit code"
                    , show c
                    , "while recursively zifting with"
                    , toFilePath scriptPath
                    ]

findZiftFilesRecursively :: Zift [Path Abs File]
findZiftFilesRecursively = do
    rd <- getRootDir
    let filterZiftFiles = filter ((== $(mkRelFile "zift.hs")) . filename) -- TODO generalise to given predicate
    let recurser absdir dirs files =
            if absdir == rd
                then pure $ WalkExclude []
                else do
                    let ziftFiles = filterZiftFiles files
                    case ziftFiles of
                        [] -> pure $ WalkExclude [] -- No zift files found, recurse further downward.
                        -- Zift files found, run each of them but don't recurse further. That's their job.
                        _ -> pure $ WalkExclude dirs
    let outputWriter absdir _ files =
            pure $
            if absdir == rd
                then []
                else filterZiftFiles files
    walkDirAccum (Just recurser) outputWriter rd
