{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

-- | Some basic types for propositional logic
module Types
       (
         TruthValue(..)
       , Literal(..)
       , opposite
       , Clause(..)
       , Con(..)
       , Cnf(..)
       , Dnf(..)
       , getClauses
       , getConjunctions
       , Table(..)
       , getTable
       , Allocation
       , genLiteral
       , genClause
       , genCon
       , genCnf
       , genDnf
       , possibleAllocations
       , Formula(..)
       , ResStep(..)
       , PrologLiteral(..)
       , PrologClause(..)
       , terms
       ) where



import qualified Data.Set as Set
import qualified SAT.MiniSat as Sat

import Data.Either(rights)
import Data.List(transpose, nub, delete, intersperse)
import Data.Set (Set,empty)
import Data.Typeable
import GHC.Generics
import Test.QuickCheck





newtype ResStep = Res {trip :: (Either Clause Int, Either Clause Int, (Clause, Maybe Int))}

newtype TruthValue = TruthValue {truth :: Bool} deriving (Typeable, Generic)



class Formula a where
    convert :: a -> Sat.Formula Char
    literals :: a -> [Literal]
    atomics :: a -> [Literal]
    amount :: a -> Int
    evaluate :: Allocation -> a -> Maybe Bool


---------------------------------------------------

-- | A datatype representing a literal
data Literal
    = Literal { letter :: Char} -- ^ positive sign
    | Not { letter :: Char} -- ^ negative sign
    deriving
      ( Eq -- ^ derived
      , Typeable -- ^ derived
      , Generic -- ^ derived
      )


-- | order literals alphabetically first, then prefer a positive sign
instance Ord Literal where
   compare (Not x) (Literal y) = if x == y then LT else compare x y
   compare (Literal x) (Not y) = if x == y then GT else compare x y
   compare l1 l2 = compare (letter l1) (letter l2)


-- | '~' denotes a negative sign
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

   amount _ = 1

   evaluate xs (Not y) = not <$> evaluate xs (Literal y)
   evaluate xs z = lookup z xs


instance Arbitrary Literal where
   arbitrary = genLiteral ['A'..'Z']


-- | Generates a literal with random sign from the given list of chars.
--   throws an error if called with the empty list.
genLiteral :: [Char] -> Gen Literal
genLiteral [] = error "Can not construct Literal from empty list."
genLiteral lits = do
   rChar <- elements lits
   elements [Literal rChar, Not rChar]


-- | Reverses the sign of the literal
opposite :: Literal -> Literal
opposite (Literal l) = Not l
opposite (Not l) = Literal l


------------------------------------------------------------

-- | A datatype representing a clause
newtype Clause = Clause { literalSet :: Set Literal}
    deriving (Eq,Typeable,Generic)



instance Ord Clause where
   compare (Clause set1) (Clause set2)
       | size1 /= size2 = compare size1 size2
       | otherwise = compare set1 set2
     where
       size1 = Set.size set1
       size2 = Set.size set2



instance Show Clause where
   show (Clause set) = listShow (Set.toList set)
     where
       listShow :: [Literal] -> String
       listShow [] = "{ }"
       listShow [x] = show x
       listShow (x:xs) = show x ++ " \\/ " ++ listShow xs



instance Formula Clause where
   convert (Clause set)
        | Set.null set = Sat.No
        | otherwise = Sat.Some (map convert (Set.toList set))

   literals (Clause set) = Set.toList set

   atomics (Clause set) = concat $ Set.toList $ Set.map atomics set

   amount (Clause set) = Set.size set

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


-- | Generates a random clause. The length of the generated clause lies in the given length bounds.
--   The used atomic formulae are drawn from the list of chars.
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


-- | A shorthand representing an allocation.
type Allocation = [(Literal, Bool)]


--------------------------------------------------------------

-- | A datatype representing a formula in conjunctive normalform.
newtype Cnf = Cnf { clauseSet :: Set Clause}
     deriving (Eq,Typeable,Generic)



instance Ord Cnf where
   compare (Cnf set1) (Cnf set2)
       | size1 /= size2 = compare size1 size2
       | otherwise = compare set1 set2
     where
       size1 = Set.size set1
       size2 = Set.size set2




-- | Retrieves the contained clauses of the cnf.
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
        listShow [] = " "
        listShow [x] = show x
        listShow (x:xs) = "(" ++ show x ++ ") /\\ (" ++ listShow xs ++ ")"


instance Formula Cnf where
    convert (Cnf set)
        | Set.null set = Sat.Yes
        | otherwise = Sat.All (map convert (Set.toList set))

    literals (Cnf set) = Set.toList $ Set.unions $ Set.map (Set.fromList . literals) set

    atomics (Cnf set) = Set.toList $ Set.unions $ Set.map (Set.fromList . atomics) set

    amount (Cnf set) = Set.size set

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




-- | Generates a random cnf satisfying the given bounds
--   for the amount and the length of the contained clauses.
--   The used atomic formulae are drawn from the list of chars.

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

-- | A datatype representing a conjunction
newtype Con = Con { literalSet :: Set Literal}
    deriving (Eq,Typeable,Generic)



instance Ord Con where
   compare (Con set1) (Con set2)
       | size1 /= size2 = compare size1 size2
       | otherwise = compare set1 set2
     where
       size1 = Set.size set1
       size2 = Set.size set2



instance Show Con where
   show (Con set) = listShow (Set.toList set)
     where
       listShow :: [Literal] -> String
       listShow [] = " "
       listShow [x] = show x
       listShow (x:xs) = show x ++ " /\\ " ++ listShow xs



instance Formula Con where
   convert (Con set)
        | Set.null set = Sat.Yes
        | otherwise = Sat.All (map convert (Set.toList set))

   literals (Con set) = Set.toList set

   atomics (Con set) = concat $ Set.toList $ Set.map atomics set

   amount (Con set) = Set.size set

   evaluate xs ys = and <$> sequence lits
     where
       lits = map (evaluate xs) (literals ys)





partEvalCon :: Con -> (Literal,Bool) -> Either Bool Con
partEvalCon (Con set) x
    | Set.null set = Left True
    | isIn || negIsIn =
     if snd x then if isIn then Right (Con setWithout) else Right (Con set)
              else if isIn then Left False else Right (Con setWithoutNeg)
    | otherwise = Right (Con set)
  where
    next = fst x
    negNext = opposite next
    isIn = next `Set.member` set
    negIsIn = negNext `Set.member` set
    setWithout = Set.delete next set
    setWithoutNeg = Set.delete negNext set



instance Arbitrary Con where
   arbitrary = sized disj
     where
       disj :: Int -> Gen Con
       disj 0 = genCon (0,0) []
       disj n = genCon (1,maxBound) (take n ['A'..'Z'])


-- | Generates a random conjunction. The length of the generated conjunction lies in the given length bounds.
--   The used atomic formulae are drawn from the list of chars.
genCon :: (Int,Int) -> [Char] -> Gen Con
genCon (minlen,maxlen) lits
    | null lits || minlen > length nLits || invalidLen = pure (Con empty)
    | otherwise = do
        len <- choose (minlen,minimum [length nLits, maxlen])
        genLits <- generateLiterals nLits empty len
        pure (Con genLits)
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

-- | A datatype representing a formula in disjunctive normalform.
newtype Dnf = Dnf { clauseSet :: Set Con}
     deriving (Eq,Typeable,Generic)



instance Ord Dnf where
   compare (Dnf set1) (Dnf set2)
       | size1 /= size2 = compare size1 size2
       | otherwise = compare set1 set2
     where
       size1 = Set.size set1
       size2 = Set.size set2




-- | Retrieves the contained conjunctions of the dnf.
getConjunctions :: Dnf -> [Con]
getConjunctions (Dnf set) = Set.toList set




partEvalDnf :: Dnf -> (Literal,Bool) -> Either Bool Dnf
partEvalDnf dnf tup
    | fst tup `notElem` lits = Right dnf
    | otherwise = result (thin applied)
  where
    lits = literals dnf
    disjs = getConjunctions dnf
    applied = map (`partEvalCon` tup) disjs
    thin :: [Either Bool Con] -> [Either Bool Con]
    thin [] = []
    thin (x:xs) =
      case x of Left False   -> thin xs
                Left True    -> [Left True]
                Right disj -> Right disj : thin xs
    result :: [Either Bool Con] -> Either Bool Dnf
    result xs
      | null xs = Left False
      | Left True `elem` xs = Left True
      | otherwise = Right (Dnf (Set.fromList (rights xs)))


instance Show Dnf where
    show (Dnf set) = listShow (Set.toList set)
      where
        listShow :: [Con] -> String
        listShow [] = " "
        listShow [x] = show x
        listShow (x:xs) = "(" ++ show x ++ ") \\/ (" ++ listShow xs ++ ")"


instance Formula Dnf where
    convert (Dnf set)
        | Set.null set = Sat.No
        | otherwise = Sat.Some (map convert (Set.toList set))

    literals (Dnf set) = Set.toList $ Set.unions $ Set.map (Set.fromList . literals) set

    atomics (Dnf set) = Set.toList $ Set.unions $ Set.map (Set.fromList . atomics) set

    amount (Dnf set) = Set.size set

    evaluate alloc dnf = or <$> sequence cons
      where
        cons = map (evaluate alloc) (getConjunctions dnf)



instance Arbitrary Dnf where
    arbitrary = sized dnf
      where
        dnf :: Int -> Gen Dnf
        dnf 0 = genDnf (0,0) (0,0) []
        dnf n = do
            minLen <- choose (1,n)
            let
              lits = take n ['A'..'Z']
              maxLen = length lits
            genDnf (1,maxLen ^2) (minLen,maxLen) lits




-- | Generates a random dnf satisfying the given bounds
--   for the amount and the length of the contained conjunctions.
--   The used atomic formulae are drawn from the list of chars.

genDnf :: (Int,Int) -> (Int,Int) -> [Char] -> Gen Dnf
genDnf (minNum,maxNum) (minLen,maxLen) lits
    | null nLits || invalidLen || invalidNum = pure (Dnf empty)
    | otherwise = do
        num <- choose (minNum, minimum [maxNum,upperBound])
        dnf <- generateCons nLits empty num
        pure (Dnf dnf)
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

    generateCons :: [Char] -> Set Con -> Int -> Gen (Set Con)
    generateCons usedLits set num
        | Set.size set == num = pure set
        | otherwise = do
            con <- genCon (minLen,maxLen) usedLits
            generateCons usedLits (Set.insert con set) num


------------------------------------------------------------

-- | A datatype representing a truth table
data Table = Table
    { getLiterals :: [Literal]
    , getEntries :: [Maybe Bool]
    } deriving (Ord,Typeable,Generic)



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
            pure (getTable (cnf :: Cnf))


-- | Returns all possible allocations for the list of literals.
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



-- | Constructs a truth table for the given formula
getTable :: Formula a => a -> Table
getTable f = Table lits values
  where
    lits = atomics f
    values = map (`evaluate` f) $ possibleAllocations lits




-------------------------------------------------------------------

data PrologLiteral = PrologLiteral
    { polarity :: Bool
    , name :: String
    , constants :: [String]
    } deriving (Eq,Typeable,Generic)


instance Ord PrologLiteral where
  compare (PrologLiteral b1 n1 f1) (PrologLiteral b2 n2 f2)
      | n1 /= n2 = compare n1 n2
      | f1 /= f2 = compare f1 f2
      | otherwise = compare b1 b2


instance Show PrologLiteral where
  show p = begin ++ name p ++ "(" ++ separated ++ ")" ++ end
    where separated = concat $ intersperse "," $ constants p
          (begin,end) = if polarity p then ("","") else ("not(",")")

newtype PrologClause = PrologClause {pliterals :: Set PrologLiteral} deriving (Eq,Typeable,Generic,Show)


terms :: PrologClause -> [PrologLiteral]
terms (PrologClause set) = Set.toList set
