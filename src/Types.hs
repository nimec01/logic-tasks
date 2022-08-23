{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

-- | Some basic types for propositional logic
module Types
       (
         Literal(..)
       , Clause(..)
       , Cnf(..)
       , genLiteral
       , genClause
       , genCnf
       ) where

import qualified Data.Set as Set

import Data.List(nub, delete)
import Data.Set (Set,empty)
import Data.Typeable
import GHC.Generics
import Test.QuickCheck

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



-- | Generates a literal with random sign from the given list of chars.
--   throws an error if called with the empty list.
genLiteral :: [Char] -> Gen Literal
genLiteral [] = error "Can not construct Literal from empty list."
genLiteral lits = do
   rChar <- elements lits
   elements [Literal rChar, Not rChar]



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


--------------------------------------------------------------

-- | A datatype representing a formula in conjunctive normalform.
newtype Cnf = Cnf { clauseSet :: Set Clause}
     deriving (Eq,Typeable,Generic)


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
