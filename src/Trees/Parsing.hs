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
import ParsingHelpers (lexeme, whitespace)

leafE :: Parser (SynTree o Char)
leafE =
    Leaf <$> lexeme (satisfy isLetter)

notE :: Parser (SynTree BinOp Char)
notE = do
    lexeme $ string showOperatorNot
    Not <$> parserT

parserTToS :: Parser (SynTree BinOp Char)
parserTToS = do
   lexeme $ char '('
   e <- parserS
   lexeme $ char ')'
   return e

parserT :: Parser (SynTree BinOp Char)
parserT = leafE <|> parserTToS <|> notE

parserS :: Parser (SynTree BinOp Char)
parserS = do
    firstT <- parserT
    foldr1 (<|>) (map (\o -> lexeme (string $ showOperator o) >> Binary o firstT <$> parserT) allBinaryOperators) <|>
      return firstT

formulaParse :: String -> Either ParseError (SynTree BinOp Char)
formulaParse = parse (whitespace >> parserS <* eof) ""
