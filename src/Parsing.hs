{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parsing (
  parser
  ) where
import Types
import Text.ParserCombinators.Parsec
import Formula
import Control.Monad (void)

trailSpaces :: Parser a -> Parser a
trailSpaces p = p <* spaces


withSpaces :: Char -> Parser Char
withSpaces = trailSpaces . char


parseOr :: Parser ()
parseOr = (trailSpaces (void $ string "\\/") <?> "Disjunction") <|> fail "Could not parse a disjunction (\\/)"



parseAnd :: Parser ()
parseAnd = (trailSpaces (void $ string "/\\") <?> "Conjunction") <|> fail "Could not parse a conjunction (/\\)"

class Parse a where
  parser :: Parser a

instance Parse a => Parse [a] where
  parser = (trailSpaces listParse <?> "List")
           <|> fail "Could not parse a list of values: The elements of a list are enclosed by square brackets '[ ]' and separated by commas."
    where
      listParse = do
        withSpaces '[' <|> fail "could not parse an opening '['"
        xs <- parser `sepBy` (withSpaces ',' <|> fail "parsed a wrong separator: Lists are comma-separated.")
        withSpaces ']' <|> fail "could not parse an enclosing ']'"
        pure xs

instance Parse Literal where
  parser = (trailSpaces litParse <?> "Literal")
           <|> fail "Could not parse a literal: Literals are denoted by capital letters, negation is denoted by a '~'."
    where
      litParse = do
        result <- optionMaybe $ char '~'
        var <- satisfy $ flip elem ['A'..'Z']
        case result of Nothing -> pure (Literal var)
                       Just _  -> pure (Not var)

instance Parse Clause where
 parser = (trailSpaces clauseParse <?> "Clause")
          <|> fail "Could not parse a clause: Clauses are composed out of literals and the 'or operator' (\\/)."
   where
     clauseParse = do
       braces <- trailSpaces $ optionMaybe $ char '('
       lits <- sepBy parser parseOr
       case braces of Nothing -> pure ' '
                      Just _ -> char ')'
       pure $ mkClause lits

instance Parse Cnf where
  parser = (trailSpaces parseCnf <?> "CNF")
           <|> fail "Could not parse a CNF: CNFs are composed out of clauses and the 'and operator' (/\\)."
    where
      parseCnf = do
        clauses <- sepBy parser parseAnd
        pure $ mkCnf clauses

