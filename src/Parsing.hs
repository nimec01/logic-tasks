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
    trailSpaces $ char '('
    clause1 <- parseEither resClause parseNum
    trailSpaces $ char ','
    clause2 <- parseEither resClause parseNum
    trailSpaces $ char ','
    clause3 <- resClause
    index <- optionMaybe indexParse
    trailSpaces $ char ')'
    return $ Res (clause1,clause2,(clause3,index))

   where
    braces = between (trailSpaces $ char '{') (trailSpaces $ char '}')

    indexParse = trailSpaces (char '=') >> trailSpaces parseNum

    resClause = mkClause <$> braces (parser `sepBy` (char ','))

    parseEither x y = trailSpaces ((Left <$> try x) <|> (Right <$> y))

    parseNum = do
      i <- many1 digit
      return (read i)




trailSpaces :: Parser a -> Parser a
trailSpaces p = p <* spaces



parseOr :: Parser ()
parseOr = trailSpaces $ void $ string "\\/"



parseAnd :: Parser ()
parseAnd = trailSpaces $ void $ string "/\\"




class Parse a where
  parser :: Parser a



instance Parse a => Parse [a] where
  parser = trailSpaces listParse
    where
      listParse = do
        trailSpaces $ char '['
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
       braces <- trailSpaces $ optionMaybe $ char '('
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
        trailSpaces $ char ','
        index <- trailSpaces $ many1 digit
        text <- optionMaybe $ trailSpaces extraText
        char ')'
        return $ PickInst cnfs (read index) text
          where
            extraText = between start (char '}') $ many1 $ satisfy ( /= '}')
            start = do
              char ','
              spaces
              char '{'

