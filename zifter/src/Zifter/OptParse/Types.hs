module Zifter.OptParse.Types where

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandRun
    | CommandInstall
    deriving (Show, Eq)

data Flags = Flags
    { flagsOutputColor :: Bool
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchRun
    | DispatchInstall
    deriving (Show, Eq)

data Settings = Settings
    { setsOutputColor :: Bool
    } deriving (Show, Eq)
