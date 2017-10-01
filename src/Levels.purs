module Levels(levels) where

import Types (Level, Type(..))

-- list of all levels
levels :: Array Level
levels = [level1, level2]

level1 :: Level
level1 = {inputs: [f,a], output: Base "B", name: "Application"} where
  f = {name: "f", typ: Function (Base "A") (Base "B")}
  a = {name: "a", typ: Base "A"}

level2 :: Level
level2 = {inputs: [f,a], output: Base "B", name: "Curry"} where
  f = {name: "f", typ: Function (Base "A") (Function (Base "A") (Base "B"))}
  a = {name: "a", typ: Base "A"}
