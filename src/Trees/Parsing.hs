{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Trees.Parsing (
  parserS,
  formulaParse,
  parsePropForm
  ) where

import Text.Parsec.Char (char, satisfy, string)
import Text.Parsec (eof, ParseError, parse, (<|>), optionMaybe)
import Text.Parsec.String (Parser)

import Data.Char (isLetter, isSpace)
import Trees.Types (SynTree(..), BinOp(..), PropFormula(..), showOperator, showOperatorNot, allBinaryOperators)
import ParsingHelpers (brackets, lexeme, whitespace)

leafE :: Parser (SynTree o Char)
leafE =
    Leaf <$> lexeme (satisfy isLetter)

notE :: Parser (SynTree BinOp Char)
notE = do
    lexeme $ string showOperatorNot
    Not <$> parserT

parserT :: Parser (SynTree BinOp Char)
parserT = leafE <|> brackets parserS <|> notE

parserS :: Parser (SynTree BinOp Char)
parserS = do
    firstT <- parserT
    foldr1 (<|>) (map (\o -> lexeme (string $ showOperator o) >> Binary o firstT <$> parserT) allBinaryOperators) <|>
      return firstT

formulaParse :: String -> Either ParseError (SynTree BinOp Char)
formulaParse = parse (whitespace >> parserS <* eof) ""


--------------------------------------

-- Parsers for formulas with reduced brackets

parseAtomic :: Parser (PropFormula Char)
parseAtomic = do
  c <- lexeme $ satisfy (\x -> isLetter x || isSpace x)
  pure $ Atomic c



parseNeg :: Parser (PropFormula Char)
parseNeg = do
  lexeme $ char '~'
  Neg <$> parseBasic



parseOp :: BinOp -> Parser BinOp
parseOp o = do
  lexeme $ string $ showOperator o
  pure o



parseAnyOp :: Parser BinOp
parseAnyOp = foldr1 (<|>) (map parseOp allBinaryOperators)



parseBrackets :: Parser (PropFormula Char)
parseBrackets = Brackets <$> brackets parsePropForm



parseBasic :: Parser (PropFormula Char)
parseBasic = parseAtomic <|> parseBrackets <|> parseNeg



parsePropForm :: Parser (PropFormula Char)
parsePropForm = do
   form1 <- parseBasic
   mOp <- optionMaybe parseAnyOp
   case mOp of
     Nothing   -> pure form1
     (Just op) ->
       do Assoc op form1 <$> parsePropForm
