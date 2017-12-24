{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | The main 'Zifter' module.
--
-- In most cases this should be the only module you import to start writing a
-- @zift.hs@ script. You will most likely want to import the appropriate
-- modules from the 'zifter-*' companion packages.
module Zifter
    ( ziftWith
    , ziftWithSetup
      -- * Defining your own zift scripts
    , preprocessor
    , prechecker
    , checker
    , ziftP
    , mapZ
    , mapZ_
    , forZ
    , forZ_
    , recursiveZift
    , ZiftScript
    , renderZiftSetup
      -- * Defining your own zift actions
    , Zift
    , getRootDir
    , getTmpDir
    , getSettings
    , getSetting
    , Settings(..)
      -- ** Console outputs of a zift action
      --
      -- | Because 'Zift' actions are automatically parallelised, it is important
      -- that they do not arbitrarily output data to the console.
      -- Instead, you should use these functions to output to the console.
      --
      -- The 'ziftWith' and 'ziftWithSetup' functions will take care of ensuring
      -- that the output appears linear.
    , printZift
    , printZiftMessage
    , printPreprocessingDone
    , printPreprocessingError
    , printWithColors
      -- * Utilities
      --
      -- | You will most likely not need these
    , runZiftAuto
    , runZift
    , ziftRunner
    , outputPrinter
    ) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
       (TChan, TMVar, atomically, newEmptyTMVar, newTChanIO, orElse,
        putTMVar, readTChan, takeTMVar, tryReadTChan)
import Control.Monad
import Path
import Path.IO
import Safe
import System.Console.ANSI
import qualified System.Directory as D
       (canonicalizePath, getPermissions, setOwnerExecutable,
        setPermissions)
import System.Environment (getProgName)
import System.Exit
import qualified System.FilePath as FP (joinPath, splitPath)
import System.IO
       (BufferMode(NoBuffering), hFlush, hSetBuffering, stderr, stdout)

import Zifter.OptParse
import Zifter.Recurse
import Zifter.Script
import Zifter.Setup
import Zifter.Zift

-- | Run a 'ZiftScript' to create the 'ZiftSetup', and then use 'ziftWithSetup'
--
-- > ziftWith = renderZiftSetup >=> ziftWithSetup
ziftWith :: ZiftScript () -> IO ()
ziftWith = renderZiftSetup >=> ziftWithSetup

-- | Build a zifter using a 'ZiftSetup'.
--
-- A zifter has the capabilities that you would expect from a 'zift.hs' file:
--
-- * @zift.hs run@:         Run the @zift.hs@ script as a pre-commit hook.
-- * @zift.hs preprocess@:  Run the preprocessor
-- * @zift.hs precheck@:    Run the prechecker
-- * @zift.hs check@:       Run the checker
-- * @zift.hs install@:     Install the @zift.hs@ script as a pre-commit hook.
ziftWithSetup :: ZiftSetup -> IO ()
ziftWithSetup setup = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    (d, sets) <- getInstructions
    case d of
        DispatchRun -> run setup sets
        DispatchPreProcess -> runPreProcessor setup sets
        DispatchPreCheck -> runPreChecker setup sets
        DispatchCheck -> runChecker setup sets
        DispatchInstall r -> install r sets

run :: ZiftSetup -> Settings -> IO ()
run ZiftSetup {..} =
    runZiftAuto $ \_ -> do
        runAsPreProcessor ziftPreprocessor
        runAsPreChecker ziftPreChecker
        runAsChecker ziftChecker

runPreProcessor :: ZiftSetup -> Settings -> IO ()
runPreProcessor ZiftSetup {..} =
    runZiftAuto $ \_ -> runAsPreProcessor ziftPreprocessor

runPreChecker :: ZiftSetup -> Settings -> IO ()
runPreChecker ZiftSetup {..} =
    runZiftAuto $ \_ -> runAsPreChecker ziftPreChecker

runChecker :: ZiftSetup -> Settings -> IO ()
runChecker ZiftSetup {..} = runZiftAuto $ \_ -> runAsChecker ziftChecker

runZiftAuto :: (ZiftContext -> Zift ()) -> Settings -> IO ()
runZiftAuto func sets = do
    rd <- autoRootDir
    td <- resolveDir rd ".zifter"
    pchan <- newTChanIO
    let ctx =
            ZiftContext
            { rootdir = rd
            , tmpdir = td
            , settings = sets
            , printChan = pchan
            , recursionList = []
            }
    result <- runZift ctx (func ctx)
    code <-
        case result of
            ZiftFailed err -> do
                outputOne (setsOutputColor sets) $
                    ZiftOutput [SetColor Foreground Dull Red] err
                pure $ ExitFailure 1
            ZiftSuccess () -> pure ExitSuccess
    exitWith code

runZift :: ZiftContext -> Zift a -> IO (ZiftResult a)
runZift ctx zfunc = do
    fmvar <- atomically newEmptyTMVar
    printerAsync <-
        async $
        outputPrinter (deriveOutputSets $ settings ctx) (printChan ctx) fmvar
    runnerAsync <- async $ ziftRunner ctx fmvar zfunc
    result <- wait runnerAsync
    wait printerAsync
    pure result

ziftRunner :: ZiftContext -> TMVar () -> Zift a -> IO (ZiftResult a)
ziftRunner ctx fmvar zfunc =
    withSystemTempDir "zifter" $ \d ->
        withCurrentDir d $ do
            r <- zift zfunc ctx
            atomically $ putTMVar fmvar ()
            pure r

deriveOutputSets :: Settings -> OutputSets
deriveOutputSets Settings {..} =
    OutputSets {outputColor = setsOutputColor, outputMode = setsOutputMode}

data OutputSets = OutputSets
    { outputColor :: Bool
    , outputMode :: OutputMode
    } deriving (Show, Eq)

outputPrinter :: OutputSets -> TChan ZiftToken -> TMVar () -> IO ()
outputPrinter OutputSets {..} =
    (case outputMode of
         OutputLinear -> outputLinear
         OutputFast -> outputFast)
        outputColor

outputFast :: Bool -> TChan ZiftToken -> TMVar () -> IO ()
outputFast = outputLinear

outputLinear :: Bool -> TChan ZiftToken -> TMVar () -> IO ()
outputLinear color pchan fmvar =
    let printer = do
            mdone <-
                atomically $
                (Left <$> takeTMVar fmvar) `orElse` (Right <$> readTChan pchan)
            case mdone of
                Left () -> outputAll
                Right output -> do
                    outputOneToken output
                    printer
    in printer
  where
    outputOneToken :: ZiftToken -> IO ()
    outputOneToken (TokenDone _) = pure ()
    outputOneToken (TokenOutput _ zo) = outputOne color zo
    outputAll = do
        mout <- atomically $ tryReadTChan pchan
        case mout of
            Nothing -> pure ()
            Just output -> do
                outputOneToken output
                outputAll

outputOne :: Bool -> ZiftOutput -> IO ()
outputOne color (ZiftOutput commands str) = do
    when color $ setSGR commands
    putStr str
    when color $ setSGR [Reset]
    putStr "\n" -- Because otherwise it doesn't work?
    hFlush stdout

runAsPreProcessor :: Zift () -> Zift ()
runAsPreProcessor func = do
    printZiftMessage "PREPROCESSOR STARTING"
    func
    printZiftMessage "PREPROCESSOR DONE"

runAsPreChecker :: Zift () -> Zift ()
runAsPreChecker func = do
    printZiftMessage "PRECHECKER STARTING"
    func
    printZiftMessage "PRECHECKER DONE"

runAsChecker :: Zift () -> Zift ()
runAsChecker func = do
    printZiftMessage "CHECKER STARTING"
    func
    printZiftMessage "CHECKER DONE"

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

install :: Bool -> Settings -> IO ()
install recursive sets = do
    autoRootDir >>= installIn
    if recursive
        then flip runZiftAuto sets $ \_ ->
                 recursively $ \ziftFile -> liftIO $ installIn $ parent ziftFile
        else pure ()

installIn :: Path Abs Dir -> IO ()
installIn rootdir = do
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
    mc <- forgivingAbsence $ readFile $ toFilePath preComitFile
    let hookContents = "./zift.hs run\n"
    let justDoIt = do
            writeFile (toFilePath preComitFile) hookContents
            pcf <- D.getPermissions (toFilePath preComitFile)
            D.setPermissions (toFilePath preComitFile) $
                D.setOwnerExecutable True pcf
            putStrLn $
                unwords
                    ["Installed pre-commit script in", toFilePath preComitFile]
    case mc of
        Nothing -> justDoIt
        Just "" -> justDoIt
        Just c ->
            if c == hookContents
                then putStrLn $
                     unwords ["Hook already installed for", toFilePath rootdir]
                else die $
                     unlines
                         [ "Not installing, a pre-commit hook already exists:"
                         , show c
                         ]

dotGitDir :: Path Rel Dir
dotGitDir = $(mkRelDir ".git")

dotGitFile :: Path Rel File
dotGitFile = $(mkRelFile ".git")

hooksDir :: Path Rel Dir
hooksDir = $(mkRelDir "hooks")
