module Tasks.SuperfluousBrackets.Parsing (
  superfluousBracketsExcParser
  ) where

import Text.Parsec.String (Parser)
import Text.Parsec (eof, ParseError, parse, many1, (<|>))
import Text.Parsec.Char (satisfy, string)
import Parsing (whitespace, lexeme)

import Data.Char (isLetter)
import Trees.Types (showOperator, showOperatorNot, allBinaryOperators)

parseLettertoStr :: Parser String
parseLettertoStr = do
    letter <- satisfy isLetter
    return [letter]

operatorAndLeavesParse :: Parser String
operatorAndLeavesParse = do
    listOfString <- many1 . lexeme $ parseLettertoStr <|> foldr (<|>) (string "(" <|> string ")" <|> string showOperatorNot) (map (string . showOperator) allBinaryOperators)
    return (concat listOfString)

superfluousBracketsExcParser :: String -> Either ParseError String
superfluousBracketsExcParser = parse (whitespace >> operatorAndLeavesParse <* eof) ""
