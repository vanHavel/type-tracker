module Abs where

import Prelude((<>))
import Data.Show(class Show, show)

data Term = Var String | Application Term Term | Lambda String Term

instance showTerm :: Show Term where
  show (Var x) = x
  show (Lambda x t) = "\\x -> " <> show t
  show (Application t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"
