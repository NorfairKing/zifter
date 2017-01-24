module Zifter.OptParse.Types where

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandRun
    | CommandInstall
    | CommandPreProcess
    | CommandCheck
    deriving (Show, Eq)

newtype Flags = Flags
    { flagsOutputColor :: Bool
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchRun
    | DispatchInstall
    | DispatchPreProcess
    | DispatchCheck
    deriving (Show, Eq)

newtype Settings = Settings
    { setsOutputColor :: Bool
    } deriving (Show, Eq)
