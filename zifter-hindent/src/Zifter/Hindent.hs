module Zifter.Hindent where

import Control.Monad.IO.Class
import Data.Foldable
import Path
import Path.IO
import Safe
import System.Exit (ExitCode(..))
import qualified System.FilePath as FP (splitPath)
import System.IO
import System.Process

import Zifter.Zift

newtype HindentBin =
    HindentBin (Path Abs File)
    deriving (Show, Eq)

hindentZift :: Zift ()
hindentZift = do
    hindentBin <- getHindent
    () <- hindentCheckAndPrintVersion hindentBin
    rd <- getRootDir
    fs <- liftIO $ snd <$> listDirRecur rd
    let sources = filter (not . hidden) $ filter ((== ".hs") . fileExtension) fs
    for_ sources $ hindentSingleSource hindentBin

getHindent :: Zift HindentBin
getHindent = do
    home <- liftIO getHomeDir
    file <- liftIO $ parseRelFile ".local/bin/hindent"
    pure $ HindentBin $ home </> file

hindentCmd :: HindentBin -> [String] -> String
hindentCmd (HindentBin ap) args = unwords $ toFilePath ap : args

hindentCheckAndPrintVersion :: HindentBin -> Zift ()
hindentCheckAndPrintVersion hb = do
    let cmd = hindentCmd hb ["--version"]
    (_, mouth, _, ph) <-
        liftIO $ createProcess ((shell cmd) {std_out = CreatePipe})
    ec <- liftIO $ waitForProcess ph
    case mouth of
        Nothing -> pure ()
        Just outh -> liftIO (hGetContents outh) >>= printZift
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()

hindentSingleSource :: HindentBin -> Path Abs File -> Zift ()
hindentSingleSource hb file = do
    let cmd =
            hindentCmd
                hb
                ["--indent-size", "4", "--line-length", "80", toFilePath file]
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
