{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parsing (
  formulaSymbol,
  whitespace,
  lexeme,
  ) where

import Text.Parsec.Char (oneOf, satisfy)
import Data.Char (isLetter)
import Text.Parsec (many, (<|>))
import Text.Parsec.String (Parser)

formulaSymbol :: Parser Char
formulaSymbol = satisfy isLetter <|> oneOf " ()\\/<=>~"

whitespace :: Parser ()
whitespace = do
    many $ oneOf " \n\t"
    return ()

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x
