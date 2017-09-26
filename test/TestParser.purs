module TestParser(testParser) where

import Abs (Term(..))
import TestUtil(assert)
import Control.Lazy (fix)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Gen (oneOf, sized, suchThat)
import Data.Array (all)
import Data.Char.Gen (genAlphaLowercase)
import Data.Char.Unicode (isAlphaNum)
import Data.Either (Either(..))
import Data.NonEmpty ((:|))
import Data.String (fromCharArray, toCharArray)
import Data.String.Gen (genAsciiString)
import Parse (runParseTerm)
import Prelude (Unit, bind, discard, pure, show, unit, ($), (&&), (+), (<$>), (<<<), (<>), (==), (>=))
import Test.QuickCheck (class Arbitrary, QC, Result(..), arbitrary, quickCheck, (<?>), (==?))
import Test.QuickCheck.Gen (resize)

-- newtype wrapper for arbirary instance
newtype TestTerm = TestTerm Term
-- unpack function for wrapper
unpack :: TestTerm -> Term
unpack (TestTerm t) = t

-- recipe to generate an arbitraty term as one of the three constructors with initial size 0
instance arbTerm :: Arbitrary TestTerm where
  arbitrary = resize 0 $ fix \a -> oneOf $ (varGen) :| [appGen a, absGen a] where
    -- arbitrary variable name
    nameGen = do
      c <- genAlphaLowercase
      s <- oneOf $ (genAsciiString `suchThat` ((all isAlphaNum) <<< toCharArray)) :| [pure ""]
      pure $ (fromCharArray [c]) <> s
    -- arbitrary variable term
    varGen = do
      name <- nameGen
      pure $ TestTerm $ Var name
    -- arbitrary application, but do not generate more than 10 nested applications to avoid stack overflow.
    appGen a = sized
      \size -> if size >= 10 then varGen else resize (size + 1) do
        (TestTerm t1) <- a
        (TestTerm t2) <- a
        pure $ TestTerm $ Application t1 t2
    -- arbitrary abstraction, also with size restriction to avoid arbitrary nested abstractions
    absGen a = sized
      \size -> if size >= 10 then varGen else resize (size + 1) do
        varName <- nameGen
        (TestTerm t) <- a
        pure $ TestTerm $ Lambda varName t

-- property saying that terms are parsed correctly
parserCorrectness :: TestTerm -> Boolean
parserCorrectness (TestTerm t) = runParseTerm (show t) == Right t

-- testing the parser with quickCheck and some manual corner cases
testParser :: forall e. QC e Unit
testParser = do
  -- using the show function of terms to check that they are currently parsed
  quickCheck (\t -> let result = runParseTerm (show $ unpack t) in
    parserCorrectness t <?> "Error: Term " <> show (unpack t) <> "parsed as" <> show result)
  -- some corner cases not handled by showing the terms
  -- whitespace
  let t1 = Right (Var "a")
  assert $ runParseTerm " a " ==? t1
  assert $ runParseTerm " a" ==? t1
  assert $ runParseTerm "a " ==? t1
  -- whitespace and dot
  let t2 = Right (Lambda "x" (Var "a"))
  assert $ runParseTerm "\\x. a " ==? t2
  assert $ runParseTerm "\\x .a " ==? t2
  assert $ runParseTerm "\\x . a " ==? t2
  assert $ runParseTerm "\\x.a " ==? t2
  -- whitespace and ->
  assert $ runParseTerm "\\x-> a " ==? t2
  assert $ runParseTerm "\\x ->a " ==? t2
  assert $ runParseTerm "\\x->a " ==? t2
  -- application is left associative
  assert $ runParseTerm "a b c" ==? Right (Application (Application (Var "a") (Var "b")) (Var "c"))
  -- lambda scope goes as far right as possible
  assert $ runParseTerm "\\x -> a b" ==? Right (Lambda "x" (Application (Var "a") (Var "b")))
  assert $ runParseTerm "(\\x -> a) b" ==? Right (Application (Lambda "x" (Var "a")) (Var "b"))
