{-# language RecordWildCards #-}

module Parsing where


import Config
import Formula
import Types

import Text.ParserCombinators.Parsec




parseOr :: Parser ()
parseOr = do
    spaces
    string "\\/"
    spaces


parseAnd :: Parser ()
parseAnd = do
    spaces
    string "/\\"
    spaces


parseList :: Parser a -> Parser [a]
parseList p = do
    spaces
    char '['
    spaces
    xs <- p `sepBy` (char ',')
    spaces
    char ']'
    return xs


-- Instances of ToDoc and Reader




class Reader a where
  reader :: Parser a



instance Reader Number where
  reader = do
      result <- optionMaybe $ many1 digit
      return $ Number $ fmap read result




instance Reader Literal where
  reader = do
      spaces
      result <- optionMaybe $ char '~'
      var <- satisfy $ flip elem ['A'..'Z']
      spaces
      case result of Nothing -> return (Literal var)
                     Just _  -> return (Not var)




instance Reader Clause where
 reader = do
    spaces
    braces <- optionMaybe $ char '('
    spaces
    lits <- sepBy reader parseOr
    spaces
    case braces of Nothing -> return ' '
                   Just _ -> char ')'
    spaces
    return $ mkClause lits



instance Reader Cnf where
  reader = do
      spaces
      clauses <- sepBy reader parseAnd
      spaces
      return $ mkCnf clauses



instance Reader Table



instance Reader PickInst where
  reader = do
      string "PickInst("
      spaces
      cnfs <- parseList reader
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




instance Reader BaseConfig
instance Reader CnfConfig
instance Reader FillInst
instance Reader DecideInst
instance Reader GiveInst
instance Reader StepInst
instance Reader ResolutionInst





