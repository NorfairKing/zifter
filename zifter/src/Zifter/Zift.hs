module Zifter.Zift
    ( getRootDir
    , getSettings
    , getSetting
    , ziftP
    , printPreprocessingDone
    , printPreprocessingError
    , printWithColors
    , liftIO
    , module Zifter.Zift.Types
    ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Foldable

import System.Console.ANSI
import System.IO

import Path

import Zifter.OptParse.Types
import Zifter.Zift.Types

getRootDir :: Zift (Path Abs Dir)
getRootDir = Zift $ \zc -> pure $ ZiftSuccess $ rootdir zc

getSettings :: Zift Settings
getSettings = Zift $ \zc -> pure $ ZiftSuccess $ settings zc

getSetting :: (Settings -> a) -> Zift a
getSetting func = func <$> getSettings

ziftP :: [Zift ()] -> Zift ()
ziftP = sequenceA_

getPrintVar :: Zift (MVar ())
getPrintVar = Zift $ \zc -> pure $ ZiftSuccess $ printvar zc

printPreprocessingDone :: String -> Zift ()
printPreprocessingDone = printWithColors [SetColor Foreground Dull Green]

printPreprocessingError :: String -> Zift ()
printPreprocessingError = printWithColors [SetColor Foreground Dull Red]

printWithColors :: [SGR] -> String -> Zift ()
printWithColors commands str = do
    color <- getSetting setsOutputColor
    pv <- getPrintVar
    liftIO $
        withMVar pv $ \() -> do
            when color $ setSGR commands
            putStr str
            when color $ setSGR [Reset]
            putStr "\n" -- Because otherwise it doesn't work?
            hFlush stdout
