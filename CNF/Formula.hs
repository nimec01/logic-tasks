module Formula where

import Test.QuickCheck
import Data.List (nub)


type Allocation = [(Literal, Bool)]

---------------------------------------------------------------------------------------------------

data Literal = Literal { getC :: Char} | Not { getC :: Char} deriving (Eq,Ord)

instance Show Literal where
 show (Literal x) = [x]
 show (Not x) = "not(" ++ [x] ++ ")" 


evalLiteral :: Allocation -> Literal -> Maybe Bool
evalLiteral [] _ = Nothing
evalLiteral xs (Not y) = not <$> evalLiteral xs (Literal y) 
evalLiteral ((x,y):xs) z = if x == z then Just y else evalLiteral xs z 


genLiteral :: [Char] -> Gen Literal
genLiteral [] = error "Can not construct Literal from empty list." 
genLiteral lits = do 
 rChar <- elements lits
 rInt <- arbitrarySizedIntegral 
 if even rInt then return (Literal rChar)
              else return (Not rChar)

---------------------------------------------------------------------------------------------------

newtype Clause = Clause { getLs :: [Literal]} deriving (Eq,Ord)

instance Show Clause where
 show (Clause []) = "False"
 show (Clause [x]) = show x
 show (Clause (x:xs)) = show x ++ " OR " ++ show (Clause xs) 


evalClause :: Allocation -> Clause -> Maybe Bool
evalClause xs ys = or <$> sequence literals
 where literals = map (evalLiteral xs) (getLs ys)


genClause :: (Int,Int) -> [Char] -> Gen Clause
genClause (minlen,maxlen) lits
 | null lits || minlen < 0 || maxlen > length lits = return (Clause [])
 | otherwise = do
  len <- choose (minlen,maxlen)
  clause <- suchThat (vectorOf len (genLiteral lits)) 
   (\c -> not (or [Literal lit `elem` c && Not lit `elem` c | lit <- lits]) && length (nub [getC x | x <- c]) == len)
  return (Clause clause)

---------------------------------------------------------------------------------------------------

newtype CNF = CNF { getCs :: [Clause]} deriving (Eq,Ord)

instance Show CNF where
 show (CNF []) = "False"
 show (CNF [x]) = show x 
 show (CNF (x:xs)) = "(" ++ show x ++ ") AND (" ++ show (CNF xs)++ ")" 


evalCNF :: Allocation -> CNF -> Maybe Bool
evalCNF xs ys = and <$> sequence clauses
 where clauses = map (evalClause xs) (getCs ys)


genCNF :: (Int,Int) -> (Int,Int) -> [Char] -> Gen CNF
genCNF (minNum,maxNum) (minLen,maxLen) lits = do
 num <- choose (minNum,maxNum)
 cnf <- suchThat (vectorOf num (genClause (minLen,maxLen) lits)) 
  (\c -> length c == length (nub c)  )
 return (CNF cnf)