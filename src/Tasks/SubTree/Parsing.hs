{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Tasks.SubTree.Parsing (
  subTreeStringParse,
  subFormulasStringParse,
  ) where

import Trees.Types (SynTree, BinOp)

import Text.Parsec.String (Parser)
import Text.Parsec (eof, ParseError, parse, sepBy, many1)
import Text.Parsec.Char (char)
import Parsing (whitespace, lexeme, formulaSymbol)
import Trees.Parsing (parserS)

import Data.Set (fromList, Set)

subTreeParse :: Parser (Set (SynTree BinOp Char))
subTreeParse = do
    lexeme $ char '{'
    subTreelist <- parserS `sepBy` lexeme (char ',')
    lexeme $ char '}'
    return $ fromList subTreelist

subTreeStringParse :: String -> Either ParseError (Set (SynTree BinOp Char))
subTreeStringParse = parse (whitespace >> subTreeParse <* eof) ""

subFormulasParse :: Parser (Set String)
subFormulasParse = do
    char '{'
    subFormulasList <- many1 formulaSymbol `sepBy` char ','
    char '}'
    return $ fromList subFormulasList

subFormulasStringParse :: String -> Either ParseError (Set String)
subFormulasStringParse = parse (subFormulasParse <* eof) ""
