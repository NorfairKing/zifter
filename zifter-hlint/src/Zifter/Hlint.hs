{-# LANGUAGE ScopedTypeVariables #-}

module Zifter.Hlint where

import Control.Exception
import Control.Monad
import Language.Haskell.HLint3 (hlint)
import Path
import Path.IO
import Safe
import System.Exit
import qualified System.FilePath as FP

import Zifter.Zift

hlintZift :: Zift ()
hlintZift = do
    rd <- getRootDir
    fs <- liftIO $ snd <$> listDirRecur rd
    let sources = filter (not . hidden) $ filter ((== ".hs") . fileExtension) fs
    void $ liftIO $ hlint ["--version"] `catch` (\(_ :: ExitCode) -> pure [])
    hints <- liftIO $ hlint ("--quiet" : map toFilePath sources)
    case hints of
        [] -> printPreprocessingDone "Hlint checks done."
        _ -> do
            printPreprocessingError $
                unlines $ "Hlint has suggestions." : map show hints
            fail "Hlint had suggestions."

hidden :: Path Abs t -> Bool
hidden = any ((Just '.' ==) . headMay) . FP.splitPath . toFilePath
