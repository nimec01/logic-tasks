{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Parsing(
  normParse
) where
import Types
import Text.Parsec (ParseError)
import Text.Parsec.String ( Parser )
import Text.ParserCombinators.Parsec(try)
import qualified Text.Parsec.Char as C
import Control.Applicative ((<|>), many)
import Data.Char (isLetter)
import qualified Text.Parsec as P

-- import FunctionsAndTypesForParsing

eof :: Parser ()
eof = P.eof

parse :: Parser a -> P.SourceName -> String -> Either P.ParseError a
parse = P.parse

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""
whitespace :: Parser ()
whitespace = do
  many $ C.oneOf " \n\t"
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
            a <- C.satisfy isLetter
            return $ Leaf  a
notE :: Parser SynTree
notE = lexeme $ do
            lexeme $ C.char '~'
            lexeme $ C.char '('
            e <- simpleExpr
            lexeme $ C.char ')'
            return $ Not e
parenthE :: Parser SynTree
parenthE = lexeme $ do
            lexeme $ C.char '('
            e <- simpleExpr
            lexeme $ C.char ')'
            return e

simpleAndE :: Parser SynTree
simpleAndE = lexeme $ do
            left <- simpleExpr
            lexeme $ C.string "/\\"
            And left <$> simpleExpr

simpleOrE :: Parser SynTree
simpleOrE = lexeme $ do
            left <- simpleExpr
            lexeme $ C.string "\\/"
            Or left <$> simpleExpr
simpleImplE :: Parser SynTree
simpleImplE = lexeme $ do
            left <- simpleExpr
            lexeme $ C.string "=>"
            Impl left <$> simpleExpr
simpleEquiE:: Parser SynTree
simpleEquiE = lexeme $ do
            left <- simpleExpr
            lexeme $ C.string "<=>"
            Equi left <$> simpleExpr
simpleBothE :: Parser SynTree
simpleBothE= lexeme $ do
            lexeme $ C.char '('
            e <- try simpleAndE <|> try simpleOrE <|> try simpleImplE <|> simpleEquiE
            lexeme $ C.char ')'
            return e
simpleExpr :: Parser SynTree
simpleExpr = try leafE <|> try simpleBothE <|> try parenthE <|> notE

normParse :: String ->Either ParseError SynTree
normParse str = parseWithWhitespace simpleExpr str1 where
                                            str2 = '(' : str
                                            str1 = str2 ++ ")"
