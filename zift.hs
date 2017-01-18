#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
-}
{-# LANGUAGE OverloadedStrings #-}

import Zifter
import Zifter.Cabal
import Zifter.Hindent
import Zifter.Stack

main :: IO ()
main =
    ziftWith $ do
        preprocessor $ do
            hindentZift
            cabalFormatZift
        checker stackBuildZift
