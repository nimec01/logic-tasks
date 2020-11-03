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
       ) where


import Data.List (delete, nub)
import Data.Set (Set,empty)
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



smartClause :: Clause -> Literal -> Clause
smartClause (Clause set) lit
 | lit `Set.member` set || opposite lit `Set.member` set = Clause set
 | otherwise = Clause (Set.insert lit set)



evalClause :: Allocation -> Clause -> Maybe Bool
evalClause xs ys = or <$> sequence literals
  where
    literals = map (evalLiteral xs) (Set.toList (getLs ys))



genClause :: (Int,Int) -> [Char] -> Gen Clause
genClause (minlen,maxlen) lits
    | null nLits || minlen > length lits || invalidLen = pure (Clause empty)
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


smartCnf :: Cnf -> Clause -> Cnf
smartCnf (Cnf cSet) (Clause lSet)
 | Set.size lSet == 1 = Cnf (Set.map Clause updated)
 | otherwise = Cnf (Set.map Clause (if Set.size neg == 0 then Set.insert lSet cnfClauses else removeDiff))
  where
    cnfClauses = Set.map getLs cSet
    toAdd = Set.elemAt 0 lSet
    updated = Set.insert lSet (Set.map (\set -> Set.delete (opposite toAdd) set) cnfClauses)
    disj set = (Set.union set lSet) Set.\\ (Set.intersection set lSet)
    neg = Set.filter (\set -> onlyOpposites (disj set)) cnfClauses
    removeDiff = Set.map (\set -> if set `Set.member` neg then set Set.\\ disj set else set) cnfClauses


onlyOpposites :: Set Literal -> Bool
onlyOpposites set
 | Set.null set = True
 | otherwise = if opposite first `Set.member` set
                 then onlyOpposites (Set.delete (opposite first) (Set.delete first set))
                 else False
   where first = Set.elemAt 0 set



evalCnf :: Allocation -> Cnf -> Maybe Bool
evalCnf xs ys = and <$> sequence clauses
  where
    clauses = map (evalClause xs) (Set.toList (getCs ys))



getLiterals :: Cnf -> [Literal]
getLiterals cnf = Set.toList $ Set.unions $ map positive $ Set.toList (getCs cnf)
  where
    positive = Set.map turnPositive . getLs


rearrange :: Cnf -> Gen Cnf
rearrange cnf = do
    let literals = getLiterals cnf
        initial = zip literals [1..]
    newOrder <- shuffle literals
    let pairing = zip [1..] newOrder
        litSet = Set.map getLs (getCs cnf)
    let toNum = conv initial litSet
        fromNum = conv pairing toNum
    return $ Cnf (Set.map Clause fromNum)
  where
    conv :: Eq a => Ord b => [(a,b)] -> Set (Set a) -> Set (Set b)
    conv pairs = Set.map (Set.map replacer)
      where
        replacer elmn = case lookup elmn pairs of
            Just x  -> x
            Nothing -> error "not possible"


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
        | Set.size set == num = pure set
        | otherwise = do
            clause <- genClause (minLen,maxLen) usedLits
            generateClauses usedLits (Set.insert clause set) num
