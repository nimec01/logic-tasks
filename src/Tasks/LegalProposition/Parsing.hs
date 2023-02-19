{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Tasks.LegalProposition.Parsing (
    illegalPropositionStringParse
    ) where


import Data.Set (Set, fromList)
import Text.Parsec (ParseError, eof, many1, parse, sepBy)
import Text.Parsec.Char (char, digit)
import Text.Parsec.String (Parser)

import ParsingHelpers (lexeme, whitespace)




illegalPropositionParse :: Parser (Set Int)
illegalPropositionParse = do
    lexeme $ char '{'
    illegalList <- lexeme (many1 digit) `sepBy` lexeme (char ',')
    lexeme $ char '}'
    return $ fromList (map read illegalList)



illegalPropositionStringParse :: String -> Either ParseError (Set Int)
illegalPropositionStringParse = parse  (whitespace >> illegalPropositionParse <* eof) ""
