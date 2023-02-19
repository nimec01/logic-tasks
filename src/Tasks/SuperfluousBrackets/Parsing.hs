
module Tasks.SuperfluousBrackets.Parsing (
    superfluousBracketsExcParser
    ) where


import Data.Char (isLetter)
import Text.Parsec (ParseError, eof, many1, parse, (<|>))
import Text.Parsec.Char (satisfy, string)
import Text.Parsec.String (Parser)

import ParsingHelpers (lexeme, whitespace)
import Trees.Types (showOperator, showOperatorNot, allBinaryOperators)




parseLetterToStr :: Parser String
parseLetterToStr = do
    letter <- satisfy isLetter
    return [letter]



operatorAndLeavesParse :: Parser String
operatorAndLeavesParse = do
    listOfString <- many1 . lexeme $
      parseLetterToStr <|> foldr
        ((<|>) . string . showOperator)
        (string "(" <|> string ")" <|> string showOperatorNot) allBinaryOperators
    return (concat listOfString)



superfluousBracketsExcParser :: String -> Either ParseError String
superfluousBracketsExcParser = parse (whitespace >> operatorAndLeavesParse <* eof) ""
