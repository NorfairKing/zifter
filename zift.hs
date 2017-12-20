#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
    --package zifter-cabal
    --package zifter-git
    --package zifter-hindent
    --package zifter-hlint
    --package zifter-stack
    --package zifter-google-java-format
-}
import Zifter
import Zifter.Cabal
import Zifter.Git
import Zifter.GoogleJavaFormat
import Zifter.Hindent
import Zifter.Hlint
import Zifter.Stack

main :: IO ()
main =
    ziftWith $ do
        recursiveZift
        preprocessor $
            ziftP
                [ hindentZiftExcept ["zifter-stack/src/Zifter/Stack.hs"]
                , cabalFormatZift
                , googleJavaFormatZift
                ]
        prechecker gitAddAllZift
        checker $ do
            hlintZift
            stackBuildZift
