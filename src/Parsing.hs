{-# language RecordWildCards #-}

module Parsing where


import Config
import Formula
import Types

import Data.Char (toLower)
import Text.ParserCombinators.Parsec




instance Parse ResStep where

  parser = do
    char '('
    [x,y,z] <- resClause `sepBy` (char ',')
    char ')'
    return $ Res (x,y,z)

   where
    resClause = do
      char '{'
      lits <- parser `sepBy` (char ',')
      char '}'
      return $ mkClause lits





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





class Parse a where
  parser :: Parser a



instance Parse a => Parse [a] where
  parser = do
      spaces
      char '['
      spaces
      xs <- parser `sepBy` (char ',')
      spaces
      char ']'
      return xs



instance Parse Number where
  parser = do
      result <- optionMaybe $ many1 digit
      return $ Number $ fmap read result




instance Parse TruthValue where
  parser = do
      s <- getInput
      setInput (map toLower s)
      parseTrue <|> parseFalse
    where
      parseTrue = do
          try (string "wahr") <|> try (string "true") <|> string "1" <|> string "w" <|> string "t"
          return $ TruthValue True
      parseFalse = do
          try (string "falsch") <|> try (string "false") <|> string "0" <|> string "f"
          return $ TruthValue False




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
    lits <- sepBy parser parseOr
    spaces
    case braces of Nothing -> return ' '
                   Just _ -> char ')'
    spaces
    return $ mkClause lits



instance Parse Cnf where
  parser = do
      spaces
      clauses <- sepBy parser parseAnd
      spaces
      return $ mkCnf clauses



instance Parse Table



instance Parse PickInst where
  parser = do
      string "PickInst("
      spaces
      cnfs <- parser
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

