module Types where

import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Show (class Show, show)
import Prelude (class Eq, otherwise, (/=), (<>), (==))

-- polymorphic types
data Type = Base String | TypeVar String | Function Type Type
instance showType :: Show Type where
  show (Base x) = x
  show (TypeVar x) = x
  show (Function a b) = "(" <> show a <> ") -> (" <> show b <> ")"
derive instance eqType :: Eq Type



-- input to a level
type Input = {name :: String,
              typ :: Type}

-- level data structure
type Level = {inputs :: Array Input,
              output :: Type,
              name :: String}

-- context for typing
type Context = Map String Type

-- simple error monad
type Error = Either String
