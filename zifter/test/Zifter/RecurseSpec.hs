module Zifter.RecurseSpec
    ( spec
    ) where

import TestImport

import Zifter.Recurse

spec :: Spec
spec =
    describe "hiddenIn" $ do
        it "correctly identifies a subdirectory of .stack-work as hidden" $ do
            rp <- parseAbsDir "/home/user/project/"
            af <-
                parseAbsFile
                    "/home/user/project/.stack-work/downloaded/abcxyz/zift.hs"
            af `shouldSatisfy` hiddenIn rp
        it "correctly identifies a regular subdirectory as not hidden" $ do
            rp <- parseAbsDir "/home/user/project/"
            af <- parseAbsFile "/home/user/project/subdir/zift.hs"
            af `shouldNotSatisfy` hiddenIn rp
