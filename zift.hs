#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
-}
{-# LANGUAGE OverloadedStrings #-}

import Zifter
import Zifter.Hindent
import Zifter.Stack
import Zifter.Types

main :: IO ()
main =
    ziftWith $
    ZiftSetup {ziftPreprocessor = hindentZift, ziftChecker = stackBuildZift}
