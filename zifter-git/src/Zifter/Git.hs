module Zifter.Git where

import Path
import System.Exit
import System.Process

import Zifter.Zift

gitAddAllZift :: Zift ()
gitAddAllZift = do
    rd <- getRootDir
    let cmd = "git add ."
    ec <-
        liftIO $
        createProcess ((shell cmd) {cwd = Just $ toFilePath rd}) >>=
        (waitForProcess . (\(_, _, _, ph) -> ph))
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()
