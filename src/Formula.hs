module Formula
       (
         Literal(..)
       , Clause(..)
       , CNF(..)
       , Allocation
       , opposite
       , evalCNF
       , genCNF
       , genClause
       , getLiterals
       , genLiteral
       , evalLiteral
       , evalClause
       , turnPositive
       ) where


import Data.List (delete)
import Data.Set (Set,member,size,fromList,toList,empty,insert,unions)
import Test.QuickCheck
import qualified Data.Set as Set (map)


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

instance Arbitrary Literal where
  arbitrary = genLiteral ['A'..'Z']

evalLiteral :: Allocation -> Literal -> Maybe Bool
evalLiteral xs (Not y) = not <$> evalLiteral xs (Literal y)
evalLiteral xs z = lookup z xs


genLiteral :: [Char] -> Gen Literal
genLiteral [] = error "Can not construct Literal from empty list."
genLiteral lits = do
 rChar <- elements lits
 elements [Literal rChar, Not rChar]


opposite :: Literal -> Literal
opposite (Literal l) = Not l
opposite (Not l) = Literal l



turnPositive :: Literal -> Literal
turnPositive (Not x) = Literal x
turnPositive (Literal x) = Literal x

---------------------------------------------------------------------------------------------------

newtype Clause = Clause { getLs :: Set Literal}
    deriving (Eq,Ord)

instance Show Clause where
 show (Clause set) = listShow (toList set)

  where listShow [] = "False"
        listShow [x] = show x
        listShow (x:xs) = show x ++ " OR " ++ listShow xs

instance Arbitrary Clause where
  arbitrary = sized clause
    where clause 0 = genClause (0,0) []
          clause n = genClause (1,maxBound) (take n ['A'..'Z'])



evalClause :: Allocation -> Clause -> Maybe Bool
evalClause xs ys = or <$> sequence literals
 where literals = map (evalLiteral xs) (toList (getLs ys))


genClause :: (Int,Int) -> [Char] -> Gen Clause
genClause (minlen,maxlen) lits
 | null lits || minlen <= 0 || minlen > length lits || minlen > maxlen = return (Clause empty)
 | otherwise = do
  len <- chooseInt (minlen,minimum [length lits, maxlen])
  literals <- generateLiterals lits [] len
  return (Clause (fromList literals))
   where generateLiterals lits xs len
           | length xs == len = return xs
           | otherwise = do
              literal <- genLiteral lits
              generateLiterals (delete (getC literal) lits) (literal:xs) len



---------------------------------------------------------------------------------------------------

newtype CNF = CNF { getCs :: Set Clause}
     deriving (Eq,Ord)

instance Arbitrary CNF where
   arbitrary = sized cnf
    where cnf 0 = genCNF (0,0) (0,0) []
          cnf n = do
            minLen <- chooseInt (1,n)
            genCNF (1,maxBound) (minLen,maxBound) (take n ['A'..'Z'])



instance Show CNF where
 show (CNF set) = listShow (toList set)

   where listShow [] = "True"
         listShow [x] = show x
         listShow (x:xs) = "(" ++ show x ++ ") AND (" ++ listShow xs ++ ")"


evalCNF :: Allocation -> CNF -> Maybe Bool
evalCNF xs ys = and <$> sequence clauses
 where clauses = map (evalClause xs) (toList (getCs ys))



getLiterals :: CNF -> [Literal]
getLiterals cnf = toList $ unions $ map (Set.map turnPositive . getLs) $ toList (getCs cnf)



genCNF :: (Int,Int) -> (Int,Int) -> [Char] -> Gen CNF
genCNF (minNum,maxNum) (minLen,maxLen) lits
 | null lits || minLen <= 0 || minLen > length lits
   || minLen > maxLen || minNum <= 0 || minNum > maxNum
   || minNum > minLen^2 = return (CNF empty)
 | otherwise = do
  num <- chooseInt (minNum,(minimum [maxNum,minLen^2]))
  cnf <- generateClauses lits empty num
  return (CNF cnf)
   where generateClauses lits set num
            | size set == num = return set
            | otherwise = do
               clause <- genClause (minLen,maxLen) lits
               generateClauses lits (if clause `member` set then set else insert clause set) num
