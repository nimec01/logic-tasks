{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Parsing(
  normParse,
  normParse2
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
leafE = lexeme $ do
            a <- satisfy isLetter
            return $ Leaf  a

notE :: Parser SynTree
notE = lexeme $ do
   lexeme $ char '~'
   Not <$> simpleExpr

parenthE :: Parser SynTree
parenthE = lexeme $ do
            lexeme $ char '('
            e <- simpleExpr
            lexeme $ char ')'
            return e

simpleAndE :: Parser SynTree
simpleAndE = lexeme $ do
            left <- simpleExpr
            lexeme $ string "/\\"
            And left <$> simpleExpr

simpleOrE :: Parser SynTree
simpleOrE = lexeme $ do
            left <- simpleExpr
            lexeme $ string "\\/"
            Or left <$> simpleExpr

simpleImplE :: Parser SynTree
simpleImplE = lexeme $ do
            left <- simpleExpr
            lexeme $ string "=>"
            Impl left <$> simpleExpr

simpleEquiE:: Parser SynTree
simpleEquiE = lexeme $ do
            left <- simpleExpr
            lexeme $ string "<=>"
            Equi left <$> simpleExpr

simpleBothE :: Parser SynTree
simpleBothE= lexeme $ do
            lexeme $ char '('
            e <- try simpleAndE <|> try simpleOrE <|> try simpleImplE <|>simpleEquiE
            lexeme $ char ')'
            return e

simpleExpr :: Parser SynTree
simpleExpr = try leafE <|> try simpleBothE <|> try parenthE <|> notE

specBothE :: Parser SynTree
specBothE= lexeme $ try simpleAndE <|> try simpleOrE <|> try simpleImplE <|>simpleEquiE

specExpr :: Parser SynTree
specExpr = try leafE <|> try notE <|> try parenthE <|>simpleEquiE
normParse :: String ->Either ParseError SynTree
normParse= parseWithWhitespace simpleEquiE
normParse2 :: String ->Either ParseError SynTree
normParse2 = parseWithWhitespace specExpr