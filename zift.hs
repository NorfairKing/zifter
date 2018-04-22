#!/usr/bin/env stack
{- stack
    --install-ghc
    runghc
    --package zifter
    --package zifter-git
    --package zifter-hindent
    --package zifter-hlint
    --package zifter-stack
-}
import Zifter
import Zifter.Git
import Zifter.Hindent
import Zifter.Hlint
import Zifter.Stack

main :: IO ()
main =
    ziftWith $ do
        recursiveZift
        preprocessor $ hindentZiftExcept ["zifter/src/Zifter/Zift/Types.hs"]
        prechecker gitAddAllZift
        checker $ do
            hlintZift
            stackBuildZift
