{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Trees.Parsing (
  parserS,
  formulaParse,
  ) where

import Text.Parsec.Char (char, satisfy, string)
import Text.Parsec (eof, ParseError, parse, (<|>))
import Text.Parsec.String (Parser)

import Data.Char (isLetter)
import Trees.Types (SynTree(..), Op(..))
import Parsing (lexeme, whitespace)

leafE :: Parser (SynTree o Char)
leafE =
    Leaf <$> lexeme (satisfy isLetter)

notE :: Parser (SynTree Op Char)
notE = do
    lexeme $ char '~'
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
    (lexeme (string "/\\") >> Binary And firstT <$> parserT) <|>
      (lexeme (string "\\/") >> Binary Or firstT <$> parserT) <|>
      (lexeme (string "=>") >> Binary Impl firstT <$> parserT) <|>
      (lexeme (string "<=>") >> Binary Equi firstT <$> parserT) <|>
      return firstT

formulaParse :: String -> Either ParseError (SynTree Op Char)
formulaParse = parse (whitespace >> parserS <* eof) ""
