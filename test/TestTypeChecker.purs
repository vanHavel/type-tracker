module TestTypeChecker (testTypeChecker) where

import Abs (Term(..))
import Control.Monad.Eff.Exception (error, throwException)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map (empty, fromFoldable) as Map
import Data.Tuple (Tuple(..))
import Prelude (Unit, pure, discard, show, unit, ($), (<>))
import Test.QuickCheck (QC, Result(..), (==?))
import TestUtil (assert)
import TypeCheck (equivalent, runTypeCheck)
import Types (Context, Type(..))

-- proposition that type checking fails
typeCheckFail :: Term -> Context -> Result
typeCheckFail t c = case runTypeCheck t c of
  Left _ -> Success
  Right typ -> Failed $ "Identified type " <> show typ

-- assert that the type of a term is equivalent to another type
assertEquivalent :: forall e. Term -> Context -> Type -> QC e Unit
assertEquivalent term c typ = case runTypeCheck term c of
  Left m -> throwException $ error m
  Right termtyp -> assert (equivalent termtyp typ ==? true)

-- test the typechecker
testTypeChecker :: forall e. QC e Unit
testTypeChecker = do
  -- define some contexts
  -- c0 :: {}
  -- c1 = x :: A
  -- c2 = x :: A, y :: a
  -- c3 = x :: A, f :: A -> B, g :: B -> A
  -- c4 = x :: A, f :: a -> B, g :: b -> b
  let c0 = Map.empty
  let c1 = Map.fromFoldable [Tuple "x" (Base "A")]
  let c2 = Map.fromFoldable [Tuple "x" (Base "A"), Tuple "y" (TypeVar "a")]
  let c3 = Map.fromFoldable [Tuple "x" (Base "A"), Tuple "f" (Function (Base "A") (Base "B")),
                             Tuple "g" (Function (Base "B") (Base "A"))]
  let c4 = Map.fromFoldable [Tuple "x" (Base "A"), Tuple "f" (Function (TypeVar "a") (Base "B")),
                             Tuple "g" (Function (TypeVar "b") (TypeVar "b"))]
  -- basic variable terms
  assert $ runTypeCheck (Var "x") c1 ==? Right (Base "A")
  assert $ runTypeCheck (Var "x") c2 ==? Right (Base "A")
  assert $ runTypeCheck (Var "y") c2 ==? Right (TypeVar "a")
  assert $ typeCheckFail (Var "x") c0
  assert $ typeCheckFail (Var "y") c1
  -- applications
  assert $ runTypeCheck (Application (Var "f") (Var "x")) c3 ==? Right (Base "B")
  assert $ runTypeCheck (Application (Var "g") (Application (Var "f") (Var "x"))) c3 ==? Right (Base "A")
  assert $ typeCheckFail (Application (Var "g") (Var "a")) c3
  assert $ typeCheckFail (Application (Var "g") (Var "f")) c3
  assert $ typeCheckFail (Application (Var "a") (Var "a")) c3
  -- polymorphic applications
  assert $ runTypeCheck (Application (Var "f") (Var "x")) c4 ==? Right (Base "B")
  assert $ runTypeCheck (Application (Var "g") (Var "x")) c4 ==? Right (Base "A")
  assert $ runTypeCheck (Application (Var "g") (Application (Var "f") (Var "x"))) c4 ==? Right (Base "B")
  assert $ runTypeCheck (Application (Var "f") (Application (Var "f") (Var "x"))) c4 ==? Right (Base "B")
  assert $ runTypeCheck (Application (Var "g") (Application (Var "g") (Var "x"))) c4 ==? Right (Base "A")
  -- lambdas and scoping
  assert $ runTypeCheck (Lambda "y" (Application (Var "f") (Var "y"))) c3 ==? Right (Function (Base "A")(Base "B"))
  assert $ typeCheckFail (Lambda "y" (Application (Var "y") (Var "y"))) c0
  assert $ typeCheckFail (Lambda "y" (Application (Var "f") (Var "f"))) c0
  assert $ runTypeCheck (Lambda "x" (Application (Var "g") (Var "x"))) c3 ==? Right (Function (Base "B")(Base "A"))
  -- lambdas and polymorphism
  assertEquivalent (Lambda "y" (Var "y")) c0 (Function (TypeVar "a") (TypeVar "a"))
  assertEquivalent (Lambda "y" (Application (Var "y") (Var "x"))) c3 (Function (Function (Base "A") (TypeVar "a")) (TypeVar "a"))
  assertEquivalent (Lambda "y" (Application (Var "g") (Var "y"))) c4 (Function (TypeVar "a") (TypeVar "a"))
