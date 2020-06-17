module Def where

import System.Random
import Data.List (transpose)


type Allocation = [(Atomic, Bool)]

---------------------------------------------------------------------------------------------------

data Atomic = Atomic { getA :: Char} | Not { getA :: Char} deriving (Eq,Ord)

instance Show Atomic where
 show (Atomic x) = [x]
 show (Not x) = "not(" ++ [x] ++ ")" 

instance Random Atomic where
 randomR (lo,hi) gen = (if even rand1 then Atomic x else Not x ,new2) 
  where (rand1,new1) = next gen
        (x,new2)     = randomR (getA lo, getA hi) new1
 random gen = randomR (Atomic 'A', Atomic 'Z') gen

---------------------------------------------------------------------------------------------------

newtype Clause = Clause { getAs :: [Atomic]} deriving (Eq,Ord)

instance Show Clause where
 show (Clause []) = "False"
 show (Clause [x]) = show x
 show (Clause (x:xs)) = show x ++ " OR " ++ show (Clause xs) 

instance Random Clause where
 randomR (lo,hi) gen = (Clause (take num (randoms gen :: [Atomic])),new) 
  where (rand,new) = next gen
        (lower,upper) = (length (getAs lo), length (getAs hi))
        num          = rand `mod` (upper-lower+1) + lower
 random gen = randomR (minBound, maxBound) gen

instance Bounded Clause where
 minBound = Clause []
 maxBound = Clause [Atomic x | x <- ['A'..'Z']]

---------------------------------------------------------------------------------------------------

newtype CNF = CNF { getCs :: [Clause]} deriving (Eq,Ord)

instance Show CNF where
 show (CNF []) = "False"
 show (CNF [x]) = show x ++ ")"
 show (CNF (x:xs)) = "(" ++ show x ++ ") AND (" ++ show (CNF xs) 

instance Random CNF where
 randomR (lo,hi) gen = (CNF (take num (randoms gen :: [Clause])),new) 
  where (rand,new) = next gen
        (lower,upper) = (length (getCs lo), length (getCs hi))
        num          = rand `mod` (upper-lower+1) + lower
 random gen = randomR (minBound, maxBound) gen

instance Bounded CNF where
 minBound = CNF []
 maxBound = CNF [Clause [Atomic x] | x <- ['A'..'Z']]

---------------------------------------------------------------------------------------------------

data Table = Table {getCNF :: CNF, getAtomics :: [Atomic], getEntries :: [Maybe Bool]}

instance Show Table where
 show t = header ++ rows 
  where atomics = getAtomics t
        formatLine [] _ = [] 
        formatLine x y = foldr (\x y -> x ++ " | " ++ y) (show y) (map show x) ++ "\n"
        header = formatLine atomics $ getCNF t 
        rows = concat [formatLine x y | (x,y) <- zip (transpose $ comb (length atomics) 1) $ getEntries t]  
        comb 0 _ = []
        comb len n = [concat $ replicate n $ replicate num 0 ++ replicate num 1] ++ comb (len-1) (n*2) 
         where num = 2^(len -1)