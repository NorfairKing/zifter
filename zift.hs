#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
    --package zifter-cabal
    --package zifter-hindent
    --package zifter-stack
    --package zifter-hlint
-}
{-# LANGUAGE OverloadedStrings #-}

import Zifter
import Zifter.Cabal
import Zifter.Hindent
import Zifter.Hlint
import Zifter.Stack

import Data.Foldable

main :: IO ()
main =
    ziftWith $ do
        preprocessor $ sequenceA_ [hindentZift, cabalFormatZift]
        checker stackBuildZift
            -- hlintZift TODO(syd) put this back when hlint is fixed to handle -XTypeApplications
