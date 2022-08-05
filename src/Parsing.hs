{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parsing (
  formulaParse,
  illegalPropositionStringParse,
  subFormulasStringParse,
  subTreeStringParse
  ) where

import Text.Parsec.Char (char, oneOf, satisfy, string, digit)
import Data.Char (isLetter)
import Text.Parsec (eof, ParseError, parse, sepBy, many, many1, (<|>))
import Data.Set (fromList, Set)

import Types (SynTree(..), Op(..))
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

leafE :: Parser (SynTree Op Char)
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
-----------------------------------------------------------------------------------
subTreeParse :: Parser (Set (SynTree Op Char))
subTreeParse = do
    lexeme $ char '{'
    subTreelist <- parserS `sepBy` lexeme (char ',')
    lexeme $ char '}'
    return $ fromList subTreelist

subTreeStringParse :: String -> Either ParseError (Set(SynTree Op Char))
subTreeStringParse = parse (whitespace >> subTreeParse <* eof) ""

subFormulasParse :: Parser (Set String)
subFormulasParse = do
    char '{'
    subFormulasList <- many1 formulaSymbol `sepBy` char ','
    char '}'
    return $ fromList subFormulasList

subFormulasStringParse :: String -> Either ParseError (Set String)
subFormulasStringParse = parse (whitespace >> subFormulasParse <* eof) ""
-----------------------------------------------------------------------------------
illegalPropositionParse :: Parser (Set Int)
illegalPropositionParse = do
    lexeme $ char '{'
    illegalList <- many1 digit `sepBy` lexeme (char ',')
    lexeme $ char '}'
    return $ fromList (map read illegalList)


illegalPropositionStringParse :: String -> Either ParseError (Set Int)
illegalPropositionStringParse = parse  (whitespace >> illegalPropositionParse <* eof) ""
