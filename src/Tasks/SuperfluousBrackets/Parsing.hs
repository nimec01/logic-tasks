module Tasks.SuperfluousBrackets.Parsing (
  superfluousBracketsExcParser
  ) where

import Text.Parsec.String (Parser)
import Text.Parsec (eof, ParseError, parse, many1, (<|>))
import Text.Parsec.Char (satisfy, string)
import Parsing (whitespace, lexeme)

import Data.Char (isLetter)
import Trees.Types (Op(..), showOperator)

parseLettertoStr :: Parser String
parseLettertoStr = do
    letter <- lexeme $ satisfy isLetter
    return [letter]

operatorAndLeavesParse :: Parser String
operatorAndLeavesParse = do
    listOfString <- many1 (parseLettertoStr <|> lexeme (string $ showOperator And) <|> lexeme (string $ showOperator Or) <|> lexeme (string $ showOperator Impl) <|> lexeme (string $ showOperator Equi) <|> lexeme (string "(") <|> lexeme (string ")") <|> lexeme (string $ showOperator Not))
    return (concat listOfString)

superfluousBracketsExcParser :: String -> Either ParseError String
superfluousBracketsExcParser = parse (whitespace >> operatorAndLeavesParse <* eof) ""
