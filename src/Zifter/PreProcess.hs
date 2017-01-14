module Zifter.PreProcess
    ( getRootDir
    , module Zifter.PreProcess.Types
    ) where

import Introduction

import Zifter.PreProcess.Types

getRootDir :: PreProcessor (Path Abs Dir)
getRootDir = PreProcessor $ \rd -> pure $ PreProcessorSuccess rd
