{-# language RecordWildCards #-}

module Parsing where


import Config
import Formula
import Types

import Text.ParserCombinators.Parsec




parserOr :: Parser ()
parserOr = do
    spaces
    string "\\/"
    spaces


parserAnd :: Parser ()
parserAnd = do
    spaces
    string "/\\"
    spaces


parserList :: Parser a -> Parser [a]
parserList p = do
    spaces
    char '['
    spaces
    xs <- p `sepBy` (char ',')
    spaces
    char ']'
    return xs






class Parse a where
  parser :: Parser a



instance Parse Number where
  parser = do
      result <- optionMaybe $ many1 digit
      return $ Number $ fmap read result




instance Parse Literal where
  parser = do
      spaces
      result <- optionMaybe $ char '~'
      var <- satisfy $ flip elem ['A'..'Z']
      spaces
      case result of Nothing -> return (Literal var)
                     Just _  -> return (Not var)




instance Parse Clause where
 parser = do
    spaces
    braces <- optionMaybe $ char '('
    spaces
    lits <- sepBy parser parserOr
    spaces
    case braces of Nothing -> return ' '
                   Just _ -> char ')'
    spaces
    return $ mkClause lits



instance Parse Cnf where
  parser = do
      spaces
      clauses <- sepBy parser parserAnd
      spaces
      return $ mkCnf clauses



instance Parse Table



instance Parse PickInst where
  parser = do
      string "PickInst("
      spaces
      cnfs <- parserList parser
      spaces
      char ','
      spaces
      index <- many1 digit
      spaces
      text <- optionMaybe extraText
      spaces
      char ')'
      return $ PickInst cnfs (read index) text
    where
      extraText = between start (char '}') $ many1 $ satisfy ( /= '}')
      start = do
          char ','
          spaces
          char '{'







