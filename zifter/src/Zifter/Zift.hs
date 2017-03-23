module Zifter.Zift
    ( getRootDir
    , getSettings
    , getSetting
    , ziftP
    , printZift
    , printZiftMessage
    , printPreprocessingDone
    , printPreprocessingError
    , printWithColors
    , addZiftOutput
    , liftIO
    , module Zifter.Zift.Types
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable

import System.Console.ANSI

import Path

import Zifter.OptParse.Types
import Zifter.Zift.Types

getContext :: Zift ZiftContext
getContext = Zift $ \zc st -> pure (ZiftSuccess zc, st)

getRootDir :: Zift (Path Abs Dir)
getRootDir = fmap rootdir getContext

getSettings :: Zift Settings
getSettings = fmap settings getContext

getSetting :: (Settings -> a) -> Zift a
getSetting func = func <$> getSettings

ziftP :: [Zift ()] -> Zift ()
ziftP = sequenceA_

printZift :: String -> Zift ()
printZift = printWithColors []

printZiftMessage :: String -> Zift ()
printZiftMessage = printWithColors [SetColor Foreground Dull Blue]

printPreprocessingDone :: String -> Zift ()
printPreprocessingDone = printWithColors [SetColor Foreground Dull Green]

printPreprocessingError :: String -> Zift ()
printPreprocessingError = printWithColors [SetColor Foreground Dull Red]

printWithColors :: [SGR] -> String -> Zift ()
printWithColors commands str = addZiftOutput $ ZiftOutput commands str

addZiftOutput :: ZiftOutput -> Zift ()
addZiftOutput zo =
    Zift $ \_ st -> do
        let st' = ZiftState {bufferedOutput = zo : bufferedOutput st}
        pure (ZiftSuccess (), st')
