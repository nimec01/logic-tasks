{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Trees.Parsing (
  parserS,
  formulaParse,
  ) where

import Text.Parsec.Char (char, satisfy, string)
import Text.Parsec (eof, ParseError, parse, (<|>))
import Text.Parsec.String (Parser)

import Data.Char (isLetter)
import Trees.Types (SynTree(..), BinOp(..), showOperator, showOperatorNot, allBinaryOperators)
import Parsing (lexeme, whitespace)

leafE :: Parser (SynTree o Char)
leafE =
    Leaf <$> lexeme (satisfy isLetter)

notE :: Parser (SynTree BinOp Char)
notE = do
    lexeme $ string showOperatorNot
    Not <$> parserT

parserTtoS :: Parser (SynTree BinOp Char)
parserTtoS = do
   lexeme $ char '('
   e <- parserS
   lexeme $ char ')'
   return e

parserT :: Parser (SynTree BinOp Char)
parserT = leafE <|> parserTtoS <|> notE

parserS :: Parser (SynTree BinOp Char)
parserS = do
    firstT <- parserT
    foldr1 (<|>) (map (\o -> lexeme (string $ showOperator o) >> Binary o firstT <$> parserT) allBinaryOperators) <|>
      return firstT

formulaParse :: String -> Either ParseError (SynTree BinOp Char)
formulaParse = parse (whitespace >> parserS <* eof) ""
