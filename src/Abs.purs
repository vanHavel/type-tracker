module Abs where

import Data.Show (class Show, show)
import Prelude ((<>), class Eq)

-- lambda terms
data Term = Var String | Application Term Term | Lambda String Term

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show (Var x) = x
  show (Lambda x t) = "(\\" <> x <> " -> " <> show t <> ")"
  show (Application t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"
