module Formula
       (
         Literal(..)
       , Clause(..)
       , Cnf(..)
       , Allocation
       , opposite
       , evalCnf
       , genCnf
       , genClause
       , getLiterals
       , genLiteral
       , evalLiteral
       , evalClause
       , turnPositive
       ) where


import Data.List (delete, nub)
import Data.Set (Set,empty)
import Test.QuickCheck
import qualified Data.Set as Set


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
   show (Clause set) = listShow (Set.toList set)
     where
       listShow :: [Literal] -> String
       listShow [] = "False"
       listShow [x] = show x
       listShow (x:xs) = show x ++ " OR " ++ listShow xs

instance Arbitrary Clause where
   arbitrary = sized clause
     where
       clause :: Int -> Gen Clause
       clause 0 = genClause (0,0) []
       clause n = genClause (1,maxBound) (take n ['A'..'Z'])



evalClause :: Allocation -> Clause -> Maybe Bool
evalClause xs ys = or <$> sequence literals
  where
    literals = map (evalLiteral xs) (Set.toList (getLs ys))



genClause :: (Int,Int) -> [Char] -> Gen Clause
genClause (minlen,maxlen) lits
    | null nLits || minlen > length lits || invalidLen = pure (Clause empty)
    | otherwise = do
        len <- chooseInt (minlen,minimum [length nLits, maxlen])
        literals <- generateLiterals nLits [] len
        pure (Clause (Set.fromList literals))
  where
    nLits = nub lits
    invalidLen = minlen > maxlen || minlen <= 0

    generateLiterals :: [Char] -> [Literal] -> Int -> Gen [Literal]
    generateLiterals usedLits xs len
        | length xs == len = pure xs
        | otherwise = do
            literal <- genLiteral usedLits
            generateLiterals (delete (getC literal) usedLits) (literal:xs) len



---------------------------------------------------------------------------------------------------

newtype Cnf = Cnf { getCs :: Set Clause}
     deriving (Eq,Ord)



instance Arbitrary Cnf where
    arbitrary = sized cnf
      where
        cnf :: Int -> Gen Cnf
        cnf 0 = genCnf (0,0) (0,0) []
        cnf n = do
            minLen <- chooseInt (1,n)
            let
              lits = take n ['A'..'Z']
              maxLen = length lits
            genCnf (1,maxLen ^2) (minLen,maxLen) lits



instance Show Cnf where
    show (Cnf set) = listShow (Set.toList set)
      where
        listShow :: [Clause] -> String
        listShow [] = "True"
        listShow [x] = show x
        listShow (x:xs) = "(" ++ show x ++ ") AND (" ++ listShow xs ++ ")"



evalCnf :: Allocation -> Cnf -> Maybe Bool
evalCnf xs ys = and <$> sequence clauses
  where
    clauses = map (evalClause xs) (Set.toList (getCs ys))



getLiterals :: Cnf -> [Literal]
getLiterals cnf = Set.toList $ Set.unions $ map positive $ Set.toList (getCs cnf)
  where
    positive = Set.map turnPositive . getLs



genCnf :: (Int,Int) -> (Int,Int) -> [Char] -> Gen Cnf
genCnf (minNum,maxNum) (minLen,maxLen) lits
    | null nLits || invalidLen || invalidNum = pure (Cnf empty)
    | otherwise = do
        num <- chooseInt (minNum, minimum [maxNum,upperBound])
        cnf <- generateClauses nLits empty num
        pure (Cnf cnf)
  where
    nLits = nub lits
    invalidLen = minLen <= 0 || minLen > maxLen || minLen > length nLits
    invalidNum = minNum <= 0 || minNum > maxNum || minNum > upperBound
    upperBound = minimum [2^maxLen, 2^length nLits]

    generateClauses :: [Char] -> Set Clause -> Int -> Gen (Set Clause)
    generateClauses usedLits set num
        | Set.size set == num = return set
        | otherwise = do
            clause <- genClause (minLen,maxLen) usedLits
            generateClauses usedLits (decide clause) num
      where
        alreadyIn :: Clause -> Bool
        alreadyIn = flip Set.member set

        decide :: Clause -> Set Clause
        decide c
            | alreadyIn c = set
            | otherwise = Set.insert c set
