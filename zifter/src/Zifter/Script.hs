module Zifter.Script
    ( preprocessor
    , prechecker
    , checker
    , module Zifter.Script.Types
    ) where

import Zifter.Script.Types
import Zifter.Setup
import Zifter.Zift

-- | Add a given zift action as a preprocessor.
preprocessor :: Zift () -> ZiftScript ()
preprocessor prep =
    ZiftScript {renderZiftScript = pure ((), mempty {ziftPreprocessor = prep})}

-- | Add a given zift action as a prechecker.
prechecker :: Zift () -> ZiftScript ()
prechecker func =
    ZiftScript {renderZiftScript = pure ((), mempty {ziftPreChecker = func})}

-- | Add a given zift action as a checker.
checker :: Zift () -> ZiftScript ()
checker ch =
    ZiftScript {renderZiftScript = pure ((), mempty {ziftChecker = ch})}
