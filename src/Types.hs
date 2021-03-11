module Types
       (
         Literal(..)
       , opposite
       , Clause(..)
       , Cnf(..)
       , getClauses
       , Table(..)
       , getTable
       , Allocation
       , genLiteral
       , genClause
       , genCnf
       , possibleAllocations
       , Formula(..)
       ) where



import qualified Data.Set as Set
import qualified SAT.MiniSat as Sat

import Test.QuickCheck

import Data.Either(rights)
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
    = Literal { letter :: Char}
    | Not { letter :: Char}
    deriving (Eq,Ord)



instance Show Literal where
   show (Literal x) = [x]
   show (Not x) = ['~', x]


instance Read Literal where
   readsPrec _ ('~':x:rest) = [(Not x, rest) | x `elem` ['A' .. 'Z']]
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



opposite :: Literal -> Literal
opposite (Literal l) = Not l
opposite (Not l) = Literal l


------------------------------------------------------------


newtype Clause = Clause { literalSet :: Set Literal}
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

   atomics (Clause set) = concat $ Set.toList $ Set.map atomics set

   evaluate xs ys = or <$> sequence lits
     where
       lits = map (evaluate xs) (literals ys)





partEvalClause :: Clause -> (Literal,Bool) -> Either Bool Clause
partEvalClause (Clause set) x
    | Set.null set = Left False
    | isIn || negIsIn =
     if snd x then if isIn then Left True else if Set.null setWithoutNeg then Left False else Right (Clause setWithoutNeg)
              else if isIn then if null setWithout then Left False else Right (Clause setWithout) else Left True
    | otherwise = Right (Clause set)
  where
    next = fst x
    negNext = opposite next
    isIn = next `Set.member` set
    negIsIn = negNext `Set.member` set
    setWithout = Set.delete next set
    setWithoutNeg = Set.delete negNext set



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
        len <- choose (minlen,minimum [length nLits, maxlen])
        genLits <- generateLiterals nLits empty len
        pure (Clause genLits)
  where
    nLits = nub lits
    invalidLen = minlen > maxlen || minlen <= 0

    generateLiterals :: [Char] -> Set Literal -> Int -> Gen (Set Literal)
    generateLiterals usedLits xs len
        | Set.size xs == len = pure xs
        | otherwise = do
            literal <- genLiteral usedLits
            let
              restLits = delete (letter literal) usedLits
              newSet = Set.insert literal xs
            generateLiterals restLits newSet len



--------------------------------------------------------------


newtype Cnf = Cnf { clauseSet :: Set Clause}
     deriving (Eq,Ord)



getClauses :: Cnf -> [Clause]
getClauses (Cnf set) = Set.toList set




partEvalCnf :: Cnf -> (Literal,Bool) -> Either Bool Cnf
partEvalCnf cnf tup
    | fst tup `notElem` lits = Right cnf
    | otherwise = result (thin applied)
  where
    lits = literals cnf
    clauses = getClauses cnf
    applied = map (`partEvalClause` tup) clauses
    thin :: [Either Bool Clause] -> [Either Bool Clause]
    thin [] = []
    thin (x:xs) =
      case x of Left False   -> [Left False]
                Left True    -> thin xs
                Right clause -> Right clause : thin xs
    result :: [Either Bool Clause] -> Either Bool Cnf
    result xs
      | Left False `elem` xs = Left False
      | null xs = Left True
      | otherwise = Right (Cnf (Set.fromList (rights xs)))


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

    literals (Cnf set) = Set.toList $ Set.unions $ Set.map (Set.fromList . literals) set

    atomics (Cnf set) = Set.toList $ Set.unions $ Set.map (Set.fromList . atomics) set

    evaluate alloc cnf = and <$> sequence clauses
      where
        clauses = map (evaluate alloc) (getClauses cnf)



instance Arbitrary Cnf where
    arbitrary = sized cnf
      where
        cnf :: Int -> Gen Cnf
        cnf 0 = genCnf (0,0) (0,0) []
        cnf n = do
            minLen <- choose (1,n)
            let
              lits = take n ['A'..'Z']
              maxLen = length lits
            genCnf (1,maxLen ^2) (minLen,maxLen) lits


genCnf :: (Int,Int) -> (Int,Int) -> [Char] -> Gen Cnf
genCnf (minNum,maxNum) (minLen,maxLen) lits
    | null nLits || invalidLen || invalidNum = pure (Cnf empty)
    | otherwise = do
        num <- choose (minNum, minimum [maxNum,upperBound])
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
    show t = unlines [ header
                     , hLine
                     , rows
                     ]
      where
        hLine = map (\c -> if c /= '|' then '-' else c) header
        lits = getLiterals t

        formatLine :: Show a => [a] -> Maybe Bool -> String
        formatLine [] _ = []
        formatLine x y =
            foldr ((\a b -> a ++ " | " ++ b) . show) (maybe "-" (show . fromEnum) y) x ++ "\n"

        header = concat [show x ++ " | " | x <- lits] ++ "F"
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
possibleAllocations lits = transpose (allCombinations lits 1)
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
    lits = atomics cnf
    values = map (`evaluate` cnf) $ possibleAllocations lits