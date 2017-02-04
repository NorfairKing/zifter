module Zifter.OptParse.Types where

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandRun
    | CommandInstall Bool -- | Recursive?
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
    | DispatchInstall Bool -- | recursive ?
    | DispatchPreProcess
    | DispatchCheck
    deriving (Show, Eq)

newtype Settings = Settings
    { setsOutputColor :: Bool
    } deriving (Show, Eq)
