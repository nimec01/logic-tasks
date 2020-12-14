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
       , convert
       , partEvalCnf
       , isPositive
       ) where


import Data.List (delete, nub)
import Data.Set (Set,empty)
import Data.Either(rights)
import Test.QuickCheck
import qualified Data.Set as Set
import qualified SAT.MiniSat as Sat



type Allocation = [(Literal, Bool)]


class SatConvertible a where
    convert :: a -> Sat.Formula Char

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

instance SatConvertible Literal where
   convert (Literal c) = Sat.Var c
   convert (Not c) = Sat.Not (Sat.Var c)



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


isPositive :: Literal -> Bool
isPositive (Not _) = False
isPositive _ = True

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

instance SatConvertible Clause where
   convert (Clause set)
        | Set.null set = Sat.No
        | otherwise = Sat.Some (map convert (Set.toList set))




evalClause :: Allocation -> Clause -> Maybe Bool
evalClause xs ys = or <$> sequence literals
  where
    literals = map (evalLiteral xs) (Set.toList (getLs ys))


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


instance SatConvertible Cnf where
    convert (Cnf set)
        | Set.null set = Sat.Yes
        | otherwise = Sat.All (map convert (Set.toList set))





evalCnf :: Allocation -> Cnf -> Maybe Bool
evalCnf xs ys = and <$> sequence clauses
  where
    clauses = map (evalClause xs) (Set.toList (getCs ys))



getLiterals :: Cnf -> [Literal]
getLiterals cnf = Set.toList $ Set.unions $ map positive $ Set.toList (getCs cnf)
  where
    positive = Set.map turnPositive . getLs


partEvalCnf :: Cnf -> (Literal,Bool) -> Either Bool Cnf
partEvalCnf cnf tup
    | fst tup `notElem` lits = Right cnf
    | otherwise = result (thin applied)
  where
    lits = getLiterals cnf
    set = getCs cnf
    applied = map (`partEvalClause` tup) (Set.toList set)
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
