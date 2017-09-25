module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit, discard)
import Test.Assert(ASSERT)

import TestParser (testParser)

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, exception :: EXCEPTION | e) Unit
main = do
  testParser
  log "You should add some more tests."
