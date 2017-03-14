{-# LANGUAGE ScopedTypeVariables #-}

module Zifter.Hlint where

import Language.Haskell.HLint3 (hlint)
import Path
import Path.IO
import Safe
import qualified System.FilePath as FP

import Zifter.Zift

hlintZift :: Zift ()
hlintZift = do
    rd <- getRootDir
    fs <- liftIO $ snd <$> listDirRecur rd
    let sources = filter (not . hidden) $ filter ((== ".hs") . fileExtension) fs
    hints <- liftIO $ hlint ("--quiet" : map toFilePath sources)
    case hints of
        [] -> printPreprocessingDone "Hlint checks done."
        _ -> do
            printPreprocessingError $
                unlines $ "Hlint has suggestions." : map show hints
            fail $ unwords ["Hlint had", show $ length hints, "suggestions."]

hidden :: Path Abs t -> Bool
hidden = any ((Just '.' ==) . headMay) . FP.splitPath . toFilePath
