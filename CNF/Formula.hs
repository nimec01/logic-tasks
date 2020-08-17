module Formula 
       (
         Literal(..)
       , Clause(..)
       , CNF(..)
       , Allocation
       , opposite
       , evalCNF
       , genCNF
       , removeDupesClauses
       , isElem
       ) where

import Test.QuickCheck (Gen,elements, chooseInt, vectorOf, suchThat, generate)
import Data.List (nub,nubBy,sort,stripPrefix, delete)


type Allocation = [(Literal, Bool)]

---------------------------------------------------------------------------------------------------

data Literal 
    = Literal { getC :: Char}
    | Not { getC :: Char}
    deriving (Eq,Ord)

instance Show Literal where
 show (Literal x) = [x]
 show (Not x) = "not(" ++ [x] ++ ")"

instance Read Literal where
  readsPrec _ ('n':'o':'t':'(':x:')':rest) = [(Not x, rest) | x `elem` ['A' .. 'Z']]
  readsPrec _ (x:rest) = [(Literal x, rest) | x `elem` ['A' .. 'Z']]
  readsPrec _ _ = []



evalLiteral :: Allocation -> Literal -> Maybe Bool
evalLiteral [] _ = Nothing
evalLiteral xs (Not y) = not <$> evalLiteral xs (Literal y)
evalLiteral ((x,y):xs) z = if x == z then Just y else evalLiteral xs z


genLiteral :: [Char] -> Gen Literal
genLiteral [] = error "Can not construct Literal from empty list."
genLiteral lits = do
 rChar <- elements lits
 rLit <- elements [Literal rChar, Not rChar]
 return rLit


opposite :: Literal -> Literal
opposite (Literal l) = Not l
opposite (Not l) = Literal l

---------------------------------------------------------------------------------------------------

newtype Clause = Clause { getLs :: [Literal]}
    deriving (Eq,Ord)

instance Show Clause where
 show (Clause []) = "False"
 show (Clause [x]) = show x
 show (Clause (x:xs)) = show x ++ " OR " ++ show (Clause xs)


evalClause :: Allocation -> Clause -> Maybe Bool
evalClause xs ys = or <$> sequence literals
 where literals = map (evalLiteral xs) (getLs ys)


genClause :: (Int,Int) -> [Char] -> Gen Clause
genClause (minlen,maxlen) lits
 | null lits || minlen < 0 || minlen > length lits = return (Clause [])
 | otherwise = do
  len <- chooseInt (minlen,minimum [length lits, maxlen])
  literals <- generateLiterals lits [] len
  return (Clause literals)
   where generateLiterals lits xs len
           | length xs == len = return xs
           | otherwise = do
              literal <- genLiteral lits
              generateLiterals (delete (getC literal) lits) (literal:xs) len 

removeDupesClauses :: [Clause] -> [Clause]
removeDupesClauses = nubBy (\c1 c2 -> sort (getLs c1) ==  sort (getLs c2))

isElem :: Clause -> [Clause] -> Bool
isElem _ [] = False
isElem y (x:xs) = if getLs y == getLs x then True else isElem y xs 

---------------------------------------------------------------------------------------------------

newtype CNF = CNF { getCs :: [Clause]}
     deriving (Eq,Ord)

instance Show CNF where
 show (CNF []) = "False"
 show (CNF [x]) = show x
 show (CNF (x:xs)) = "(" ++ show x ++ ") AND (" ++ show (CNF xs) ++ ")"


evalCNF :: Allocation -> CNF -> Maybe Bool
evalCNF xs ys = and <$> sequence clauses
 where clauses = map (evalClause xs) (getCs ys)


genCNF :: (Int,Int) -> (Int,Int) -> [Char] -> Gen CNF
genCNF (minNum,maxNum) (minLen,maxLen) lits = do
 num <- chooseInt (minNum,maxNum)
 cnf <- generateClauses lits [] num
 return (CNF cnf)
  where generateClauses lits xs num
           | length xs == num = return xs
           | otherwise = do
              clause <- genClause (minLen,maxLen) lits
              generateClauses lits (if isElem clause xs then xs else clause:xs) num


removeDupesCNFs :: [CNF] -> [CNF]
removeDupesCNFs = nubBy (\c1 c2 -> map (sort . getLs) (getCs c1) ==  map (sort . getLs) (getCs c2))
