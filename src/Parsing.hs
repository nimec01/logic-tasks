{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
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

lexeme :: Parser a -> Parser a--作用为读取后去掉所有空格
lexeme p = do
           x <- p
           whitespace
           return x

leafE :: Parser SynTree
leafE = do
            a <- satisfy isLetter
            return $ Leaf  a

notE :: Parser SynTree
notE =  do
   lexeme $ char '~'
   Not <$> parserT

simpleAndE :: Parser SynTree
simpleAndE = do
            left <- parserT
            lexeme $ string "/\\"
            And left <$> parserT

simpleOrE :: Parser SynTree
simpleOrE = do
            left <- parserT
            lexeme $ string "\\/"
            Or left <$> parserT

simpleImplE :: Parser SynTree
simpleImplE = do
            left <- parserT
            lexeme $ string "=>"
            Impl left <$> parserT

simpleEquiE:: Parser SynTree
simpleEquiE = do
            left <- parserT
            lexeme $ string "<=>"
            Equi left <$> parserT

parserTtoS :: Parser SynTree
parserTtoS=  do
            lexeme $ char '('
            e <- parserS
            lexeme $ char ')'
            return e

parserT :: Parser SynTree
parserT = try leafE <|>try parserTtoS  <|> notE

parserBothT :: Parser SynTree
parserBothT= try simpleAndE <|> try simpleOrE <|> try simpleImplE <|>simpleEquiE

parserS :: Parser SynTree
parserS = try  parserBothT <|> parserT

normParse :: String ->Either ParseError SynTree
normParse = parseWithWhitespace parserS