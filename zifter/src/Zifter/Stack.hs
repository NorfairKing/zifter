{-# LANGUAGE TemplateHaskell #-}

module Zifter.Stack where

import Introduction

import Control.Monad.Fail

import System.Process (system)

import Zifter.Zift

stackBuildZift :: Zift ()
stackBuildZift = do
    () <- stackCheckAndPrintVersion
    stackBuild

stackCheckAndPrintVersion :: Zift ()
stackCheckAndPrintVersion = do
    let cmd = "stack --version"
    ec <- liftIO $ system cmd
    case ec of
        ExitFailure c -> fail $ unwords [cmd, "failed with exit code", show c]
        ExitSuccess -> pure ()

stackBuild :: Zift ()
stackBuild = do
    let cleanCmd = "stack clean zifter zifter-cabal zifter-hindent" -- TODO read the cabal file to find the target.
    cec <- liftIO $ system cleanCmd
    case cec of
        ExitFailure c ->
            fail $ unwords [cleanCmd, "failed with exit code", show c]
        ExitSuccess -> pure ()
    let buildCmd = "stack build --pedantic"
    bec <- liftIO $ system buildCmd
    case bec of
        ExitFailure c ->
            fail $ unwords [buildCmd, "failed with exit code", show c]
        ExitSuccess -> pure ()
