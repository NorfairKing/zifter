{-# LANGUAGE DeriveGeneric #-}

module Zifter.OptParse.Types where

import GHC.Generics

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandRun
    | CommandInstall Bool -- | Recursive?
    | CommandPreProcess
    | CommandPreCheck
    | CommandCheck
    deriving (Show, Eq)

data Flags = Flags
    { flagsOutputColor :: Bool
    , flagsOutputMode :: Maybe OutputMode
    } deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchRun
    | DispatchInstall Bool -- | recursive ?
    | DispatchPreProcess
    | DispatchPreCheck
    | DispatchCheck
    deriving (Show, Eq, Generic)

data Settings = Settings
    { setsOutputColor :: Bool
    , setsOutputMode :: OutputMode
    } deriving (Show, Eq, Generic)

data OutputMode
    = OutputLinear
    | OutputFast
    deriving (Show, Eq, Generic)
