{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Tasks.SubTree.Parsing (
  subTreeStringParse,
  subFormulasStringParse,
  ) where



import Data.Set (Set, fromList)
import Text.Parsec (ParseError, eof, many1, parse, sepBy)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

import ParsingHelpers (formulaSymbol, lexeme, whitespace)
import Trees.Parsing (parserS)
import Trees.Types (BinOp, SynTree)




subTreeParse :: Parser (Set (SynTree BinOp Char))
subTreeParse = do
    lexeme $ char '{'
    subTreeList <- parserS `sepBy` lexeme (char ',')
    lexeme $ char '}'
    return $ fromList subTreeList



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
