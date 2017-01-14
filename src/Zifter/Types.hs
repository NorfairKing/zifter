{-# LANGUAGE DeriveGeneric #-}

module Zifter.Types where

import Introduction

import Zifter.PreProcess.Types

data ZiftSetup = ZiftSetup
    { ziftPreprocessor :: PreProcessor ()
    } deriving (Generic)
