{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parsing (
  formulaParse
  ) where

import Text.Parsec.Char (char, oneOf, satisfy, string)
import Control.Applicative ((<|>), many)
import Data.Char (isLetter)
import Text.Parsec (ParseError, eof, parse)

import Types (SynTree(..))
import Text.Parsec.String (Parser)

whitespace :: Parser ()
whitespace = do
    many $ oneOf " \n\t"
    return ()

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x

leafE :: Parser (SynTree Char)
leafE =
    Leaf <$> lexeme (satisfy isLetter)

notE :: Parser (SynTree Char)
notE = do
    lexeme $ char '~'
    Not <$> parserT

parserTtoS :: Parser (SynTree Char)
parserTtoS = do
   lexeme $ char '('
   e <- parserS
   lexeme $ char ')'
   return e

parserT :: Parser (SynTree Char)
parserT = leafE <|> parserTtoS <|> notE

parserS :: Parser (SynTree Char)
parserS = do
    firstT <- parserT
    (lexeme (string "/\\") >> And firstT <$> parserT) <|>
      (lexeme (string "\\/") >> Or firstT <$> parserT) <|>
      (lexeme (string "=>") >> Impl firstT <$> parserT) <|>
      (lexeme (string "<=>") >> Equi firstT <$> parserT) <|>
      return firstT

formulaParse :: String -> Either ParseError (SynTree Char)
formulaParse = parse (whitespace >> parserS <* eof) ""
