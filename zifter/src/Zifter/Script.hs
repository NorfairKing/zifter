module Zifter.Script
    ( preprocessor
    , precheck
    , checker
    , module Zifter.Script.Types
    ) where

import Zifter.Script.Types
import Zifter.Setup
import Zifter.Zift

preprocessor :: Zift () -> ZiftScript ()
preprocessor prep =
    ZiftScript {renderZiftScript = pure ((), mempty {ziftPreprocessor = prep})}

precheck :: Zift () -> ZiftScript ()
precheck func =
    ZiftScript {renderZiftScript = pure ((), mempty {ziftPreCheck = func})}

checker :: Zift () -> ZiftScript ()
checker ch =
    ZiftScript {renderZiftScript = pure ((), mempty {ziftChecker = ch})}
