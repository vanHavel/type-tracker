module Main where

import Prelude (Unit, ($))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Parse (runParseTerm)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ runParseTerm "a b"
