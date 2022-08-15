{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Tasks.LegalProposition.Parsing (
  illegalPropositionStringParse
  ) where

import Text.Parsec.String (Parser)
import Text.Parsec (eof, ParseError, parse, sepBy, many1)
import Text.Parsec.Char (char, digit)
import Parsing (whitespace, lexeme)

import Data.Set (fromList, Set)

illegalPropositionParse :: Parser (Set Int)
illegalPropositionParse = do
    lexeme $ char '{'
    illegalList <- lexeme (many1 digit) `sepBy` lexeme (char ',')
    lexeme $ char '}'
    return $ fromList (map read illegalList)


illegalPropositionStringParse :: String -> Either ParseError (Set Int)
illegalPropositionStringParse = parse  (whitespace >> illegalPropositionParse <* eof) ""
