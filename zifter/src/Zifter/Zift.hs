module Zifter.Zift
    ( getRootDir
    , getSettings
    , getSetting
    , printWithColors
    , liftIO
    , module Zifter.Zift.Types
    ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import System.Console.ANSI

import Path

import Zifter.OptParse.Types
import Zifter.Zift.Types

getRootDir :: Zift (Path Abs Dir)
getRootDir = Zift $ \zc -> pure $ ZiftSuccess $ rootdir zc

getSettings :: Zift Settings
getSettings = Zift $ \zc -> pure $ ZiftSuccess $ settings zc

getSetting :: (Settings -> a) -> Zift a
getSetting func = func <$> getSettings

printWithColors :: [SGR] -> String -> Zift ()
printWithColors commands str = do
    color <- getSetting setsOutputColor
    liftIO $ do
        when color $ setSGR commands
        putStr str
        when color $ setSGR [Reset]
        putStr "\n" -- Because otherwise it doesn't work?
