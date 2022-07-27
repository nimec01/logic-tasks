{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parsing (
  formulaParse,
  illegalPropositionStringParse,
  subFormulasStringParse
  ) where

import Text.Parsec.Char (char, oneOf, satisfy, string, digit)
import Data.Char (isLetter)
import Text.Parsec (eof, ParseError, parse, sepBy, many, many1, (<|>))
import Data.Set (fromList, Set)

import Types (SynTree(..))
import Text.Parsec.String (Parser)

formulaSymbol :: Parser Char
formulaSymbol = oneOf (['A'..'Z'] ++ "()/<=>~\\")

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
-----------------------------------------------------------------------------------
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
