{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ParsingHelpers (
  formulaSymbol,
  whitespace,
  lexeme,
  ) where

import Text.Parsec.Char (oneOf, satisfy)
import Data.Char (isLetter, isSpace)
import Text.Parsec (many, (<|>))
import Text.Parsec.String (Parser)
import Trees.Types (showOperator, showOperatorNot, allBinaryOperators)
import Data.List.Extra (nubOrd)

formulaSymbol :: Parser Char
formulaSymbol = satisfy isLetter <|> oneOf (nubOrd ("()" ++ showOperatorNot ++ concatMap showOperator allBinaryOperators))

whitespace :: Parser ()
whitespace = do
    many $ satisfy isSpace
    return ()

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x
