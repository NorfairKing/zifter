{-# LANGUAGE RecordWildCards #-}

module Zifter.OptParse
    ( module Zifter.OptParse
    , module Zifter.OptParse.Types
    ) where

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
    sets = Settings {setsOutputColor = flagsOutputColor}
    d =
        case cmd of
            CommandRun -> DispatchRun
            CommandInstall -> DispatchInstall

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [[Char]] -> ParserResult Arguments
runArgumentsParser = execParserPure pfs argParser
  where
    pfs =
        ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
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
        [command "run" parseCommandRun, command "install" parseCommandInstall]

parseCommandRun :: ParserInfo Command
parseCommandRun = info parser modifier
  where
    parser = pure CommandRun
    modifier = fullDesc <> progDesc "Run the zift script."

parseCommandInstall :: ParserInfo Command
parseCommandInstall = info parser modifier
  where
    parser = pure CommandInstall
    modifier = fullDesc <> progDesc "Install the zift script."

parseFlags :: Parser Flags
parseFlags = Flags <$> doubleSwitch "color" "color in output." mempty

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
