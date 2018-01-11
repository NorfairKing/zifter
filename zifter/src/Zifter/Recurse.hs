{-# LANGUAGE TemplateHaskell #-}

module Zifter.Recurse
    ( recursiveZift
    , recursively
    , hiddenIn
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Path
import Path.IO
import System.Exit
import qualified System.FilePath as FP
import System.IO
import System.Process

import Zifter.Script
import Zifter.Zift

-- | Recursively call each @zift.hs@ script in the directories below the
-- directory of the currently executing @zift.hs@ script.
--
-- Only the topmost @zift.hs@ script in each directory is executed.
-- This means that, to execute all @zift.hs@ scripts recursively, each of those
-- @zift.hs@ scripts must also have a 'recursiveZift' declaration.
recursiveZift :: ZiftScript ()
recursiveZift = do
    preprocessor $ do
        rd <- getRootDir
        printRecursionMsg $
            unwords ["RECURSIVE PREPROCESSING STARTING FROM", toFilePath rd]
        recursively $ \ziftFile -> runZiftScript ziftFile "preprocess"
        printRecursionMsg $
            unwords ["RECURSIVE PREPROCESSING FROM", toFilePath rd, "DONE."]
    prechecker $ do
        rd <- getRootDir
        printRecursionMsg $
            unwords ["RECURSIVE PRECHECKING STARTING FROM", toFilePath rd]
        recursively $ \ziftFile -> runZiftScript ziftFile "precheck"
        printRecursionMsg $
            unwords ["RECURSIVE PRECHECKING FROM", toFilePath rd, "DONE."]
    checker $ do
        rd <- getRootDir
        printRecursionMsg $
            unwords ["RECURSIVE CHECKING STARTING FROM", toFilePath rd]
        recursively $ \ziftFile -> runZiftScript ziftFile "check"
        printRecursionMsg $
            unwords ["RECURSIVE CHECKING FROM", toFilePath rd, "DONE"]

recursively :: (Path Abs File -> Zift ()) -> Zift ()
recursively func = do
    fs <- findZiftFilesRecursively
    -- In serial on purpose.
    forM_ fs func

halfIndent :: String -> String
halfIndent = ("  " ++)

indent :: String -> String
indent = halfIndent . ("| " ++)

printRecursionMsg :: String -> Zift ()
printRecursionMsg = printZiftMessage . halfIndent

runZiftScript :: Path Abs File -> String -> Zift ()
runZiftScript scriptPath command = do
    rd <- getRootDir
    printRecursionMsg $
        unwords
            [ "ZIFTING"
            , toFilePath scriptPath
            , "AS PART OF RECURSIVE ZIFT FROM"
            , toFilePath rd
            ]
    let cmd = unwords [toFilePath scriptPath, command]
    let cp =
            (shell cmd)
            {cwd = Just $ toFilePath $ parent scriptPath, std_out = CreatePipe}
    (_, mouth, merrh, ph) <- liftIO $ createProcess cp
    ec <- liftIO $ waitForProcess ph
    case mouth of
        Nothing -> pure ()
        Just outh -> do
            cts <- liftIO (hGetContents outh)
            forM_ (lines cts) $ printZift . indent
    case merrh of
        Nothing -> pure ()
        Just errh -> liftIO (hGetContents errh) >>= printZift
    case ec of
        ExitSuccess ->
            printRecursionMsg $
            unwords
                [ "ZIFTING"
                , toFilePath scriptPath
                , "AS PART OF RECURSIVE ZIFT FROM"
                , toFilePath rd
                , "DONE"
                ]
        ExitFailure c -> do
            printPreprocessingError $ halfIndent "RECURSIVE ZIFT FAILED"
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
    fs <- findFiles [rd] $(mkRelFile "zift.hs")
    pure $ filter (not . hiddenIn rd) fs

hiddenIn :: Path Abs Dir -> Path Abs File -> Bool
hiddenIn rp af =
    case stripProperPrefix rp af of
        Nothing -> True
        Just rf -> any (isPrefixOf ".") $ FP.splitDirectories $ fromRelFile rf
