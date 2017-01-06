#!/usr/bin/env stack
{- stack --install-ghc
    runghc
    --package zifter
-}
{-# LANGUAGE OverloadedStrings #-}

import Zifter

main :: IO ()
main = zift
