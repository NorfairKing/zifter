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

-- | Get the root directory of the @zift.hs@ script that is being executed.
getRootDir :: Zift (Path Abs Dir)
getRootDir = fmap rootdir getContext

-- | Get all the 'Settings'
getSettings :: Zift Settings
getSettings = fmap settings getContext

-- | Get a single setting
getSetting :: (Settings -> a) -> Zift a
getSetting func = func <$> getSettings

-- | Declare a given list of 'Zift' actions to be execute in parallel.
ziftP :: [Zift ()] -> Zift ()
ziftP = sequenceA_

-- | Print a message (with a newline appended to the end).
printZift :: String -> Zift ()
printZift = printWithColors []

-- | Print a message (with a newline appended to the end), in the standard
-- zift script color. This is the function that the zift script uses to output
-- information about the stages of the zift script run.
printZiftMessage :: String -> Zift ()
printZiftMessage = printWithColors [SetColor Foreground Dull Blue]

-- | Print a message (with a newline appended to the end) that signifies that
-- a part of the processing is now done.
--
-- Example:
--
-- > doThingZift :: Zift ()
-- > doThingZift = do
-- >     doThing
-- >     printProcessingDone "doThing completed successfully."
printPreprocessingDone :: String -> Zift ()
printPreprocessingDone = printWithColors [SetColor Foreground Dull Green]

-- | Print a message (with a newline appended to the end) that signifies that
-- a part of the processing failed. This message will not cause the zift script
-- run to fail.
--
-- Example:
--
-- > doDangerousThing :: Zift ()
-- > doDangerousThing = do
-- >     errOrResult <- doThing
-- >     case errOrResult of
-- >         Left err ->
-- >             printPreprocessingError $
-- >                 unwords ["doThing failed with error:", err]
-- >             fail "doThing failed."
-- >         Right result -> do
-- >             printPreprocessingDone
-- >                 unwords ["doThing succeeded with result:", result]
printPreprocessingError :: String -> Zift ()
printPreprocessingError = printWithColors [SetColor Foreground Dull Red]

-- | Print a message (with a newline appended to the end) with custom colors.
--
-- See the [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal)
-- package for more details.
printWithColors :: [SGR] -> String -> Zift ()
printWithColors commands str = addZiftOutput $ ZiftOutput commands str

addZiftOutput :: ZiftOutput -> Zift ()
addZiftOutput zo =
    Zift $ \_ st -> do
        let st' = ZiftState {bufferedOutput = zo : bufferedOutput st}
        pure (ZiftSuccess (), st')
