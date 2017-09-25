module Parse(runParseTerm) where

import Abs (Term(..))
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.String (singleton, fromCharArray)
import Prelude (bind, discard, pure, ($), (<$>), (<>), (>>=))
import Text.Parsing.StringParser (Parser, runParser, ParseError(..))
import Text.Parsing.StringParser.Combinators (between, many, fix, chainl1)
import Text.Parsing.StringParser.String (alphaNum, lowerCaseChar, string, whiteSpace)
import Types (Error)

-- general parser for parenthesis expressions
parens :: forall a. Parser a -> Parser a
parens = between (string "(") (string ")")

-- general parser handling surrounding whitespace
inWhiteSpace :: forall a. Parser a -> Parser a
inWhiteSpace = between (whiteSpace) (whiteSpace)

-- run the term parser and convert to error monad
runParseTerm :: String -> Error Term
runParseTerm s = let result = runParser parseTermString s in
  case result of
    (Left (ParseError m)) -> Left m
    (Right term) -> pure term

-- parse term string with surrounding whitespace
parseTermString :: Parser Term
parseTermString = inWhiteSpace parseTerm

-- parse a general term by parsing a list of nonapplications and folding them with applications
parseTerm :: Parser Term
parseTerm = fix \p -> chainl1 (parseNonApp p) (do
  _ <- whiteSpace
  pure Application)
    where
      parseNonApp p = parens p <|> parseAbstraction <|> parseVar

-- parse a lambda abstraction term
parseAbstraction :: Parser Term
parseAbstraction = do
  _ <- string "\\"
  x <- parseVarName
  _ <- whiteSpace
  _ <- string "." <|> string "->"
  _ <- whiteSpace
  t <- parseTerm
  pure $ Lambda x t

-- parse a variable identifier
parseVarName :: Parser String
parseVarName = do
  c <- lowerCaseChar
  s <- fromCharArray <$> fromFoldable <$> many alphaNum
  pure (singleton c <> s)

-- parse a variable as a term
parseVar :: Parser Term
parseVar = do
  x <- parseVarName
  pure (Var x)
