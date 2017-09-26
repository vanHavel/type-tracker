module Test.Main where

import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit, discard)
import Test.Assert (ASSERT)
import Test.QuickCheck (QC)
import TestParser (testParser)
import TestTypeChecker (testTypeChecker)

main :: forall e. QC e Unit
main = do
  testParser
  testTypeChecker
