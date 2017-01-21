{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Zifter
    ( ziftWith
    , ziftWithSetup
    , preprocessor
    , checker
    , ziftP
    , module Zifter.Script.Types
    ) where

import Control.Concurrent (newMVar)
import Control.Monad
import Path
import Path.IO
import Safe
import qualified System.Directory as D
       (canonicalizePath, setPermissions, getPermissions,
        setOwnerExecutable)
import System.Environment (getProgName)
import System.Exit (die)
import qualified System.FilePath as FP (splitPath, joinPath)
import System.IO
       (hSetBuffering, BufferMode(NoBuffering), stderr, stdout)

import System.Console.ANSI

import Zifter.OptParse
import Zifter.Script
import Zifter.Script.Types
import Zifter.Setup
import Zifter.Zift

ziftWith :: ZiftScript () -> IO ()
ziftWith = renderZiftScript >=> (ziftWithSetup . snd)

ziftWithSetup :: ZiftSetup -> IO ()
ziftWithSetup setup = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    (d, sets) <- getInstructions
    case d of
        DispatchRun -> run sets setup
        DispatchInstall -> install

run :: Settings -> ZiftSetup -> IO ()
run sets ZiftSetup {..} = do
    rd <- autoRootDir
    printvar <- newMVar ()
    let ctx = ZiftContext rd sets printvar
    withSystemTempDir "zifter" $ \d ->
        withCurrentDir d $ do
            r <-
                flip zift ctx $ do
                    printZiftMessage
                        ("CHANGED WORKING DIRECTORY TO " ++ toFilePath d)
                    runAsPreProcessor ziftPreprocessor
                    runAsChecker ziftChecker
                    printZiftMessage "ZIFTER DONE"
            case r of
                ZiftFailed err -> die err
                ZiftSuccess () -> pure ()

runAsPreProcessor :: Zift () -> Zift ()
runAsPreProcessor func = do
    printZiftMessage "PREPROCESSOR STARTING"
    func
    printZiftMessage "PREPROCESSOR DONE"

runAsChecker :: Zift () -> Zift ()
runAsChecker func = do
    printZiftMessage "CHECKER STARTING"
    func
    printZiftMessage "CHECKER DONE"

printZiftMessage :: String -> Zift ()
printZiftMessage = printWithColors [SetColor Foreground Dull Blue]

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
    ghd <-
        case (gd, gf) of
            (True, True) -> die "The .git dir is both a file and a directory?"
            (False, False) ->
                die
                    "The .git dir is nor a file nor a directory, I don't know what to do."
            (True, False) -> pure $ gitdir </> hooksDir
            (False, True) -> do
                contents <- readFile $ toFilePath gitfile
                case splitAt (length "gitdir: ") contents of
                    ("gitdir: ", rest) ->
                        case initMay rest of
                            Just gitdirref -> do
                                sp <-
                                    D.canonicalizePath $
                                    toFilePath rootdir ++ gitdirref
                                let figureOutDoubleDots =
                                        FP.joinPath . go [] . FP.splitPath
                                      where
                                        go acc [] = reverse acc
                                        go (_:acc) ("../":xs) = go acc xs
                                        go acc (x:xs) = go (x : acc) xs
                                realgitdir <-
                                    parseAbsDir $ figureOutDoubleDots sp
                                pure $ realgitdir </> hooksDir
                            Nothing ->
                                die "no gitdir reference found in .git file."
                    _ ->
                        die
                            "Found weird contents of the .git file. It is a file but does not start with 'gitdir: '. I don't know what to do."
    let preComitFile = ghd </> $(mkRelFile "pre-commit")
    putStrLn $
        unwords ["Installed pre-commit script in", toFilePath preComitFile]
    writeFile (toFilePath preComitFile) "./zift.hs run\n"
    pcf <- D.getPermissions (toFilePath preComitFile)
    D.setPermissions (toFilePath preComitFile) $ D.setOwnerExecutable True pcf

dotGitDir :: Path Rel Dir
dotGitDir = $(mkRelDir ".git")

dotGitFile :: Path Rel File
dotGitFile = $(mkRelFile ".git")

hooksDir :: Path Rel Dir
hooksDir = $(mkRelDir "hooks")
