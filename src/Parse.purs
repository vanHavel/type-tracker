module Parse where

import Abs (Term(..))
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.String (singleton, fromCharArray)
import Prelude (bind, pure, ($), (<>), (<$>))
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (between, many, fix, chainl1)
import Text.Parsing.StringParser.String (alphaNum, lowerCaseChar, string)

parens :: forall a. Parser a -> Parser a
parens = between (string "(") (string ")")

parseTerm :: Parser Term
parseTerm = fix \p -> chainl1 (parseNonApp p) (pure Application) where
  parseNonApp p = parens p <|> parseAbstraction <|> parseVar

parseAbstraction :: Parser Term
parseAbstraction = do
  _ <- string "\\"
  x <- parseVarName
  _ <- string "." <|> string "->"
  t <- parseTerm
  pure $ Lambda x t

parseVarName :: Parser String
parseVarName = do
  c <- lowerCaseChar
  s <- fromCharArray <$> fromFoldable <$> many alphaNum
  pure (singleton c <> s)

parseVar :: Parser Term
parseVar = do
  x <- parseVarName
  pure (Var x)
