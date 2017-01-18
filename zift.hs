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

main :: IO ()
main =
    ziftWith $ do
        preprocessor hindentZift
        checker stackBuildZift
