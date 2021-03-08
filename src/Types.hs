module Types
       (
         Literal(..)
       , Clause(..)
       , Cnf(..)
       , Table(..)
       , getTable
       , Allocation
       , convert
       , genLiteral
       , genClause
       , genCnf
       , possibleAllocations
       , Formula(..)
       ) where



import qualified Data.Set as Set
import qualified SAT.MiniSat as Sat

import Test.QuickCheck

import Data.List(transpose, nub, delete)
import Data.Set (Set,empty)





type Allocation = [(Literal, Bool)]

class Formula a where
    convert :: a -> Sat.Formula Char
    literals :: a -> [Literal]
    atomics :: a -> [Literal]
    evaluate :: Allocation -> a -> Maybe Bool


---------------------------------------------------

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


instance Formula Literal where
   convert (Literal c) = Sat.Var c
   convert (Not c) = Sat.Not (Sat.Var c)

   literals lit = [lit]

   atomics (Not x) = [Literal x]
   atomics lit = [lit]

   evaluate xs (Not y) = not <$> evaluate xs (Literal y)
   evaluate xs z = lookup z xs


instance Arbitrary Literal where
   arbitrary = genLiteral ['A'..'Z']



genLiteral :: [Char] -> Gen Literal
genLiteral [] = error "Can not construct Literal from empty list."
genLiteral lits = do
   rChar <- elements lits
   elements [Literal rChar, Not rChar]




------------------------------------------------------------


newtype Clause = Clause { getLs :: Set Literal}
    deriving (Eq,Ord)



instance Show Clause where
   show (Clause set) = listShow (Set.toList set)
     where
       listShow :: [Literal] -> String
       listShow [] = "False"
       listShow [x] = show x
       listShow (x:xs) = show x ++ " OR " ++ listShow xs



instance Formula Clause where
   convert (Clause set)
        | Set.null set = Sat.No
        | otherwise = Sat.Some (map convert (Set.toList set))

   literals (Clause set) = Set.toList set

   atomics (Clause set) = concatMap atomics (Set.toList set)

   evaluate xs ys = or <$> sequence lits
     where
       lits = map (evaluate xs) (literals ys)




instance Arbitrary Clause where
   arbitrary = sized clause
     where
       clause :: Int -> Gen Clause
       clause 0 = genClause (0,0) []
       clause n = genClause (1,maxBound) (take n ['A'..'Z'])


genClause :: (Int,Int) -> [Char] -> Gen Clause
genClause (minlen,maxlen) lits
    | null lits || minlen > length nLits || invalidLen = pure (Clause empty)
    | otherwise = do
        len <- chooseInt (minlen,minimum [length nLits, maxlen])
        literals <- generateLiterals nLits empty len
        pure (Clause literals)
  where
    nLits = nub lits
    invalidLen = minlen > maxlen || minlen <= 0

    generateLiterals :: [Char] -> Set Literal -> Int -> Gen (Set Literal)
    generateLiterals usedLits xs len
        | Set.size xs == len = pure xs
        | otherwise = do
            literal <- genLiteral usedLits
            let
              restLits = delete (getC literal) usedLits
              newSet = Set.insert literal xs
            generateLiterals restLits newSet len



--------------------------------------------------------------


newtype Cnf = Cnf { getCs :: Set Clause}
     deriving (Eq,Ord)




instance Show Cnf where
    show (Cnf set) = listShow (Set.toList set)
      where
        listShow :: [Clause] -> String
        listShow [] = "True"
        listShow [x] = show x
        listShow (x:xs) = "(" ++ show x ++ ") AND (" ++ listShow xs ++ ")"


instance Formula Cnf where
    convert (Cnf set)
        | Set.null set = Sat.Yes
        | otherwise = Sat.All (map convert (Set.toList set))

    literals (Cnf set) = concatMap literals (Set.toList set)

    atomics (Cnf set) = concatMap atomics (Set.toList set)

    evaluate xs ys = and <$> sequence clauses
      where
        clauses = map (evaluate xs) (Set.toList (getCs ys))



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
    lengthBound 1 len = 2*len
    lengthBound n len
        | n == maxLen && n == minLen = 2^n
        | n == minLen = 2^n * len
        | n == len = 2^n + lengthBound (n-1) len
        | otherwise = 2^n * len + lengthBound (n-1) len
    upperBound = lengthBound maxLen (length nLits)

    generateClauses :: [Char] -> Set Clause -> Int -> Gen (Set Clause)
    generateClauses usedLits set num
        | Set.size set == num = pure set
        | otherwise = do
            clause <- genClause (minLen,maxLen) usedLits
            generateClauses usedLits (Set.insert clause set) num



------------------------------------------------------------


data Table = Table
    { getLiterals :: [Literal]
    , getEntries :: [Maybe Bool]} deriving Ord



instance Eq Table where
    (==) t1 t2 = getEntries t1 == getEntries t2



instance Show Table where
    show t = header ++ "\n" ++ rows
      where
        lits = getLiterals t

        formatLine :: Show a => [a] -> Maybe Bool -> String
        formatLine [] _ = []
        formatLine x y =
            foldr ((\a b -> a ++ " | " ++ b) . show) (maybe "---" show y) x ++ "\n"

        header = concat [show x ++ " | " | x <- lits] ++ "VALUES"
        rows = concat [formatLine x y | (x,y) <- unformattedRows]
          where
            unformattedRows = zip (transpose $ comb (length lits) 1) $ getEntries t
              where
                comb :: Int -> Int -> [[Int]]
                comb 0 _ = []
                comb len n =
                    concat (replicate n $ repNum 0 ++ repNum 1) : comb (len-1) (n*2)
                  where
                    num = 2^(len -1)

                    repNum :: a -> [a]
                    repNum = replicate num



instance Arbitrary Table where
    arbitrary = sized table
      where
        table :: Int -> Gen Table
        table n = do
            cnf <- resize n arbitrary
            pure (getTable cnf)


possibleAllocations :: [Literal] -> [Allocation]
possibleAllocations xs = transpose (allCombinations xs 1)
  where
    allCombinations :: [Literal] -> Int ->  [Allocation]
    allCombinations [] _ = []
    allCombinations (x:xs) n =
      concat (replicate n $ pairs False ++ pairs True) : allCombinations xs (n*2)
      where
        num = 2^ length xs
        pairs :: a -> [(Literal,a)]
        pairs a = replicate num (x,a)


getTable :: Cnf -> Table
getTable cnf = Table lits values
  where
    lits = Set.toList $ Set.unions $ map (Set.map filterSign . getLs) $ Set.toList (getCs cnf)

    filterSign :: Literal -> Literal
    filterSign x = case x of Not y -> Literal y
                             _     -> x
    values = map (`evaluate` cnf) $ possibleAllocations lits