module Parsing(
  normParse
) where
import Types
import Text.Parsec (ParseError)
import Text.Parsec.String 
import Text.Parsec.String.Parsec (try)
import Text.Parsec.String.Char 
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter)
import FunctionsAndTypesForParsing


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
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
            a <- satisfy (isLetter) 
            return $ Leaf $ Literal a
notE :: Parser SynTree
notE = lexeme $ do
            void $ lexeme $ char '~'
            void $ lexeme $ char '('
            e <- simpleExpr
            void $ lexeme $ char ')'
            return $ Not e
parenthE :: Parser SynTree
parenthE = lexeme $ do
            void $ lexeme $ char '('
            e <- simpleExpr
            void $ lexeme $ char ')'
            return e

simpleAndE :: Parser SynTree
simpleAndE = lexeme $ do 
            left <- simpleExpr
            _ <-string "/\\"
            And left <$> simpleExpr

simpleOrE :: Parser SynTree
simpleOrE = lexeme $ do 
            left <- simpleExpr
            _ <-string "\\/"
            Or left <$> simpleExpr
simpleImplE :: Parser SynTree
simpleImplE = lexeme $ do 
            left <- simpleExpr
            _ <-string "=>"
            Impl left <$> simpleExpr
simpleEquiE:: Parser SynTree
simpleEquiE = lexeme $ do 
            left <- simpleExpr
            _ <-string "<=>"
            Equi left <$> simpleExpr
simpleBothE :: Parser SynTree
simpleBothE= lexeme $ do 
            void $ lexeme $ char '('
            e <- try simpleAndE <|> try simpleOrE <|> try simpleImplE <|> simpleEquiE
            void $ lexeme $ char ')'
            return e
simpleExpr :: Parser SynTree
simpleExpr = try leafE <|> try simpleBothE <|> try parenthE <|> notE

normParse :: String ->Either ParseError SynTree
normParse str = parseWithWhitespace simpleExpr str1 where 
                                            str2 = '(' : str
                                            str1 = str2 ++ ")"

-- remainingCells :: GenParser Char st [String]
-- remainingCells =
--     (char ',' >> cells)            -- Found comma?  More cells coming
--     <|> (return [])                -- No comma?  Return [], no more cells
--遇到括号时可以使用


-- simpleAndE :: Parser SynTree
-- simpleAndE = lexeme $ do 
--             void $ lexeme $ char '('
--             left <- simpleExpr
--             string "/\\"
--             right <- simpleExpr
--             void $ lexeme $ char ')'
--             return (And left right)

-- simpleOrE :: Parser SynTree
-- simpleOrE = lexeme $ do 
--             void $ lexeme $ char '('
--             left <- simpleExpr
--             string "\\/"
--             right <- simpleExpr
--             void $ lexeme $ char ')'
--             return (And left right)
-- simpleImplE :: Parser SynTree
-- simpleImplE = lexeme $ do 
--             void $ lexeme $ char '('
--             left <- simpleExpr
--             string "=>"
--             right <- simpleExpr
--             void $ lexeme $ char ')'
--             return (Impl left right)
-- simpleEquiE:: Parser SynTree
-- simpleEquiE = lexeme $ do 
--             void $ lexeme $ char '('
--             left <- simpleExpr
--             string "<=>"
--             right <- simpleExpr
--             void $ lexeme $ char ')'
--             return (Impl left right)

-- import Text.Parsec (ParseError)
-- import Text.Parsec.String 
-- import Text.Parsec.String.Char (anyChar)
-- import Text.Parsec.String.Char
-- import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
-- import Data.Char
-- import Text.Parsec.String.Combinator (many1)
-- import Control.Monad (void, Monad (return))
-- import Control.Applicative ((<|>), many)
-- import Foreign.C (eFTYPE)
-- import Control.Arrow (ArrowChoice(left, right))
-- import Control.Exception (try)
-- import Data.String (String)
