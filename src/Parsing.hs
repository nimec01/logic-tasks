{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE BlockArguments #-}
module Parsing(
  normParse,
) where

import Types ( SynTree(Equi, Leaf, Not, And, Or, Impl) )
import Text.Parsec.String ( Parser )
import Text.ParserCombinators.Parsec(try)
import Text.Parsec.Char ( char, oneOf, satisfy, string )
import Control.Applicative ((<|>), many)
import Data.Char (isLetter)
import Text.Parsec(ParseError,parse,eof)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""
whitespace :: Parser ()
whitespace = do
  many $ oneOf " \n\t"
  return ()
parseWithWhitespace :: Parser a -> String -> Either ParseError a
parseWithWhitespace p = parseWithEof wrapper
 where
 wrapper = do
  whitespace
  p

lexeme :: Parser a -> Parser a
lexeme p = do
 x <- p
 whitespace
 return x

leafE :: Parser SynTree
leafE = do
 a <- lexeme $ satisfy isLetter
 return $ Leaf  a

notE :: Parser SynTree
notE =  do
 lexeme $ char '~'
 Not <$> parserT

simpleBothE::(Parser String,SynTree -> SynTree -> SynTree) ->Parser SynTree
simpleBothE (bothparse,oper) = do
 left <- parserT
 lexeme bothparse
 oper left <$> parserT

parserTtoS :: Parser SynTree
parserTtoS=  do
 lexeme $ char '('
 e <- parserS
 lexeme $ char ')'
 return e

parserT :: Parser SynTree
parserT = try leafE <|>try parserTtoS  <|> notE

parserBothT :: Parser SynTree
parserBothT= (try $ simpleBothE (string "/\\",And) )<|> (try $ simpleBothE (string "\\/",Or))<|>(try $ simpleBothE (string "=>",Impl)) <|> (simpleBothE (string "<=>",Equi))

parserS :: Parser SynTree
parserS = try  parserBothT <|> parserT

normParse :: String ->Either ParseError SynTree
normParse = parseWithWhitespace parserS