module TypeCheck(runTypeCheck) where

import Data.Maybe

import Abs (Term(..))
import Control.Monad.State (lift, StateT, evalStateT, get, put)
import Data.Either (Either(..))
import Data.Functor (map)
import Data.Map (Map)
import Data.Map (lookup, empty, insert, singleton, filterKeys, member) as Map
import Prelude (pure, bind, discard, show, otherwise, not, (<>), (+), ($), (==), (||))
import Types (Type(..), Context, Error)

-- substitution of type variables
type Substitution = Map String Type

-- Milner-like type inference returns type and substitution of type variables
type TypeInference = {typ :: Type,
                      subst :: Substitution}

-- typecheck monad: Either String with a counter for fresh type variables
type TypeCheck = StateT Int Error

-- typecheck a term in a given context
runTypeCheck :: Term -> Context -> Error TypeInference
runTypeCheck t c = evalStateT (typecheck t c) 0

-- internal typecheck routine
typecheck :: Term -> Context -> TypeCheck TypeInference
typecheck (Var x) c = do
  xtype <- lift $ lookup x c
  pure ({typ: xtype, subst: Map.empty})
typecheck (Lambda x t) c = do
  typevar <- fresh
  {typ: termtype, subst: theta} <- typecheck t (Map.insert x typevar c)
  let resultType = Function (perform theta typevar) termtype
  pure {typ: resultType, subst: theta}
typecheck (Application t1 t2) c = do
  {typ: type1, subst: theta1} <- typecheck t1 c
  {typ: type2, subst: theta2} <- typecheck t2 (performOnContext theta1 c)
  typevar <- fresh
  theta3 <- lift $ unify (perform theta1 type1) (Function type2 typevar)
  pure {typ: perform theta3 typevar, subst: theta1 `compose` theta2 `compose` theta3}

-- lookup a variable in a context
lookup :: String -> Context -> Error Type
lookup s c = case Map.lookup s c of
  Nothing -> Left ("Unknown variable " <> s)
  Just typ -> pure typ

-- return a fresh type variable
fresh :: TypeCheck Type
fresh = do
  counter <- get
  put (counter + 1)
  pure (TypeVar ("t" <> show counter))

-- apply a substitution to a type
perform :: Substitution -> Type -> Type
perform theta (Base a) = (Base a)
perform theta (TypeVar a) = case Map.lookup a theta of
  Nothing -> TypeVar a
  Just atype -> atype
perform theta (Function a b) = Function (perform theta a) (perform theta b)

-- map a substitution over a context
performOnContext :: Substitution -> Context -> Context
performOnContext theta = map (perform theta)

-- compose two substitutions: apply theta first, then sigma
compose :: Substitution -> Substitution -> Substitution
compose theta sigma = let sigmaExclusives = Map.filterKeys (\a -> not $ Map.member a theta) sigma in
  map (perform sigma) theta <> sigmaExclusives

-- unify two given types
unify :: Type -> Type -> Error Substitution
unify t1 t2 = mgu t1 t2 Map.empty

-- find mgu of two types, given current substitution
mgu :: Type -> Type -> Substitution -> Error Substitution
mgu t1@(Base a) t2@(Base b) theta | a == b    = pure theta
                                  | otherwise = Left ("Failed to unify " <> show t1 <> " and " <> show t2)
mgu t1@(Base a) (TypeVar b) theta = pure $ Map.singleton b t1 `compose` theta
mgu t1@(Base a) t2@(Function b c) theta = Left ("Failed to unify " <> show t1 <> " and " <> show t2)
mgu (TypeVar a) (TypeVar b) theta | a == b    = pure theta
                                  | otherwise = pure $ Map.singleton a (TypeVar b) `compose` theta
mgu t1@(TypeVar a) t2@(Function b c) theta | occurs a t2 = Left ("Failed to unify " <> show t1 <> " and " <> show t2)
                                           | otherwise   = pure $ Map.singleton a t2 `compose` theta
mgu t1@(Function a b) t2@(Function c d) theta = do
  sigma <- mgu a c theta
  mgu b d sigma
mgu t1 t2 theta = mgu t2 t1 theta

-- occurs check of a type variable in a term
occurs :: String -> Type -> Boolean
occurs a (Base _) = false
occurs a (TypeVar b) | a == b = true
                     | otherwise = false
occurs a (Function b c) = occurs a b || occurs a c
