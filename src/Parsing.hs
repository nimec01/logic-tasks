{-# language RecordWildCards #-}

module Parsing where


import Config
import Formula
import Types

import Control.Monad (void)
import Data.Char (toLower)
import Text.ParserCombinators.Parsec




instance Parse ResStep where

  parser = do
    char '('
    clause1 <- parseEither resClause parseNum
    char ','
    clause2 <- parseEither resClause parseNum
    char ','
    clause3 <- resClause
    index <- optionMaybe indexParse
    char ')'
    return $ Res (clause1,clause2,(clause3,index))

   where
    braces = between (char '{') (char '}')

    indexParse = do
      char '='
      i <- parseNum
      return i

    resClause = do
      lits <- braces (parser `sepBy` (char ','))
      return $ mkClause lits

    parseEither x y = (Left <$> try x) <|> (Right <$> y)
    parseNum = do
      i <- many1 digit
      return (read i)



leadingSpaces :: Parser a -> Parser a
leadingSpaces p = spaces >> p


trailSpaces :: Parser a -> Parser a
trailSpaces p = do
    res <- p
    spaces
    return res


parseOr :: Parser ()
parseOr = trailSpaces $ void $ string "\\/"



parseAnd :: Parser ()
parseAnd = trailSpaces $ void $ string "/\\"




class Parse a where
  parser :: Parser a



instance Parse a => Parse [a] where
  parser = leadingSpaces $ trailSpaces listParse
    where
      listParse = do
        char '['
        spaces
        xs <- parser `sepBy` (char ',')
        char ']'
        return xs



instance Parse Number where
  parser = trailSpaces numParse
    where numParse = do
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
  parser = trailSpaces litParse
    where
      litParse = do
        result <- optionMaybe $ char '~'
        var <- satisfy $ flip elem ['A'..'Z']
        case result of Nothing -> return (Literal var)
                       Just _  -> return (Not var)




instance Parse Clause where
 parser = trailSpaces clauseParse
   where
     clauseParse = do
       braces <- optionMaybe $ char '('
       spaces
       lits <- sepBy parser parseOr
       case braces of Nothing -> return ' '
                      Just _ -> char ')'
       return $ mkClause lits



instance Parse Cnf where
  parser = trailSpaces parseCnf
    where
      parseCnf = do
        clauses <- sepBy parser parseAnd
        return $ mkCnf clauses



instance Parse Table



instance Parse PickInst where
  parser = trailSpaces instParse
    where
      instParse = do
        string "PickInst("
        cnfs <- parser
        char ','
        spaces
        index <- many1 digit
        text <- optionMaybe $ trailSpaces extraText
        char ')'
        return $ PickInst cnfs (read index) text
          where
            extraText = between start (char '}') $ many1 $ satisfy ( /= '}')
            start = do
              char ','
              spaces
              char '{'

