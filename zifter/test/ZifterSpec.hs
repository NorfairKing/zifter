{-# LANGUAGE FlexibleContexts #-}

module ZifterSpec
    ( spec
    ) where

import TestImport

import Control.Concurrent.STM

import Zifter
import Zifter.OptParse.Gen ()
import Zifter.Zift
import Zifter.Zift.Gen ()

spec :: Spec
spec =
    describe "ziftWith" $
    it "does nothing with an empty zift action" $
    forAll genUnchecked $ \sets ->
        forAll genValid $ \rd -> do
            pchan <- newTChanIO
            td <- resolveDir rd ".zifter"
            let ctx =
                    ZiftContext
                    { rootdir = rd
                    , tmpdir = td
                    , settings = sets
                    , printChan = pchan
                    , recursionList = []
                    }
            runZift ctx (pure ()) `shouldReturn` ZiftSuccess ()
