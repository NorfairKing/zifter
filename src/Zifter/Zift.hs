module Zifter.Zift
    ( getRootDir
    , module Zifter.Zift.Types
    ) where

import Introduction

import Zifter.Zift.Types

getRootDir :: Zift (Path Abs Dir)
getRootDir = Zift $ \rd -> pure $ ZiftSuccess rd
