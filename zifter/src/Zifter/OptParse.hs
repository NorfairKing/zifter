{-# LANGUAGE RecordWildCards #-}

module Zifter.OptParse
    ( module Zifter.OptParse
    , Instructions
    , Dispatch(..)
    , Settings(..)
    , OutputMode(..)
    ) where

import Data.Maybe
import Data.Monoid
import Options.Applicative
import System.Environment (getArgs)

import Zifter.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Configuration = pure (d, sets)
  where
    sets =
        Settings
        { setsOutputColor = flagsOutputColor
        , setsOutputMode = fromMaybe OutputFast flagsOutputMode
        }
    d =
        case cmd of
            CommandRun -> DispatchRun
            CommandInstall r -> DispatchInstall r
            CommandPreProcess -> DispatchPreProcess
            CommandPreCheck -> DispatchPreCheck
            CommandCheck -> DispatchCheck

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure pfs argParser
  where
    pfs =
        ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) hlp
  where
    hlp = fullDesc <> progDesc description
    description = "Zifter"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
    hsubparser $
    mconcat
        [ command "run" parseCommandRun
        , command "preprocess" parseCommandPreProcess
        , command "precheck" parseCommandPreCheck
        , command "check" parseCommandCheck
        , command "install" parseCommandInstall
        ]

parseCommandRun :: ParserInfo Command
parseCommandRun = info parser modifier
  where
    parser = pure CommandRun
    modifier = fullDesc <> progDesc "Run the zift script."

parseCommandPreProcess :: ParserInfo Command
parseCommandPreProcess = info parser modifier
  where
    parser = pure CommandPreProcess
    modifier = fullDesc <> progDesc "PreProcess according to the zift script."

parseCommandPreCheck :: ParserInfo Command
parseCommandPreCheck = info parser modifier
  where
    parser = pure CommandPreCheck
    modifier = fullDesc <> progDesc "PreCheck according to the zift script."

parseCommandCheck :: ParserInfo Command
parseCommandCheck = info parser modifier
  where
    parser = pure CommandCheck
    modifier = fullDesc <> progDesc "Check according to the zift script."

parseCommandInstall :: ParserInfo Command
parseCommandInstall = info parser modifier
  where
    parser =
        CommandInstall <$> doubleSwitch "recursive" "Install recursively" mempty
    modifier = fullDesc <> progDesc "Install the zift script."

parseFlags :: Parser Flags
parseFlags =
    Flags <$> doubleSwitch "color" "color in output." mempty <*> outputModeFlag

doubleSwitch :: String -> String -> Mod FlagFields Bool -> Parser Bool
doubleSwitch name helpText mods =
    let enabledValue = True
        disabledValue = False
        defaultValue = True
    in (last <$>
        some
            ((flag'
                  enabledValue
                  (hidden <> internal <> long name <> help helpText <> mods) <|>
              flag'
                  disabledValue
                  (hidden <> internal <> long ("no-" ++ name) <> help helpText <>
                   mods)) <|>
             flag'
                 disabledValue
                 (long ("[no-]" ++ name) <>
                  help
                      ("Enable/disable " ++
                       helpText ++ " (default: " ++ show defaultValue ++ ")") <>
                  mods))) <|>
       pure defaultValue

outputModeFlag :: Parser (Maybe OutputMode)
outputModeFlag =
    (flag'
         (Just OutputLinear)
         (mconcat [long "linear", help "output linearly, reorder as necessary."]) <|>
     flag'
         (Just OutputFast)
         (mconcat
              [ long "fast"
              , help "output as soon as possible, this is likely faster"
              ])) <|>
    pure Nothing
