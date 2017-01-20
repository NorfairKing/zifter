#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
    --package zifter-cabal
    --package zifter-hindent
    --package zifter-stack
-}
{-# LANGUAGE OverloadedStrings #-}

import Zifter
import Zifter.Cabal
import Zifter.Hindent
import Zifter.Stack

import Data.Foldable

main :: IO ()
main =
    ziftWith $ do
        preprocessor $ sequenceA_ [hindentZift, cabalFormatZift]
        checker stackBuildZift
