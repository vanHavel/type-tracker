module Types where

import Data.Either (Either)
import Data.Map (Map)
import Data.Show (class Show, show)
import Prelude (class Eq, (<>))

data Type = Base String | TypeVar String | Function Type Type
instance showType :: Show Type where
  show (Base x) = x
  show (TypeVar x) = x
  show (Function a b) = "(" <> show a <> ") -> (" <> show b <> ")"
derive instance eqType :: Eq Type

data Input = Input {name :: String,
                    type :: Type}

data Level = Level {inputs :: Array Input,
                    output :: Type}

type Context = Map String Type

type Error = Either String
