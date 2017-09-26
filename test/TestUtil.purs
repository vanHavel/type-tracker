module TestUtil (assert) where

-- assertion for simple tests
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error, throwException)
import Prelude (Unit, ($))
import Test.QuickCheck (QC, Result(..))

-- assertion for simple tests
assert :: forall e. Result -> QC e Unit
assert Success = log "Test passed"
assert (Failed m) = throwException $ error m
