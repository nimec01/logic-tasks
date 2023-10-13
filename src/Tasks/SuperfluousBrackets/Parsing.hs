
module Tasks.SuperfluousBrackets.Parsing (
    superfluousBracketsExcParser
    ) where


import Data.Functor (void)
import Text.Parsec (ParseError, parse, many, (<|>))
import Text.Parsec.String (Parser)

import ParsingHelpers (fully, tokenSymbol)
import UniversalParser (orParser, andParser, implicationParser, biImplicationParser, negationParser, atomParser)


operatorAndLeavesParse :: Parser ()
operatorAndLeavesParse =
  void . many $
      tokenSymbol "("
  <|> tokenSymbol ")"
  <|> orParser
  <|> andParser
  <|> implicationParser
  <|> biImplicationParser
  <|> negationParser
  <|> void atomParser

superfluousBracketsExcParser :: String -> Either ParseError ()
superfluousBracketsExcParser = parse (fully operatorAndLeavesParse) ""
