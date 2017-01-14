{-# LANGUAGE DeriveGeneric #-}

module Zifter.Types where

import Introduction

import Zifter.Zift.Types

data ZiftSetup = ZiftSetup
    { ziftPreprocessor :: Zift ()
    , ziftChecker :: Zift ()
    } deriving (Generic)
