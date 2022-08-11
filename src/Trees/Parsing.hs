{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Trees.Parsing (
  parserS,
  formulaParse,
  ) where

import Text.Parsec.Char (char, satisfy, string)
import Text.Parsec (eof, ParseError, parse, (<|>))
import Text.Parsec.String (Parser)

import Data.Char (isLetter)
import Trees.Types (SynTree(..), Op(..), showOperator, allBinaryOperators)
import Parsing (lexeme, whitespace)

leafE :: Parser (SynTree o Char)
leafE =
    Leaf <$> lexeme (satisfy isLetter)

notE :: Parser (SynTree Op Char)
notE = do
    lexeme $ string (showOperator Not)
    Unary Not <$> parserT

parserTtoS :: Parser (SynTree Op Char)
parserTtoS = do
   lexeme $ char '('
   e <- parserS
   lexeme $ char ')'
   return e

parserT :: Parser (SynTree Op Char)
parserT = leafE <|> parserTtoS <|> notE

parserS :: Parser (SynTree Op Char)
parserS = do
    firstT <- parserT
    foldr1 (<|>) (map (\o -> lexeme (string $ showOperator o) >> Binary o firstT <$> parserT) allBinaryOperators) <|>
      return firstT

formulaParse :: String -> Either ParseError (SynTree Op Char)
formulaParse = parse (whitespace >> parserS <* eof) ""
