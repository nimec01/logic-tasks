module Formula
       (
         Allocation
       , opposite
       , getLiterals
       , turnPositive
       , partEvalCnf
       , isPositive
       ) where


import qualified Data.Set as Set

import Data.Either(rights)

import Data(Literal(..), Clause(..), Cnf(..), Allocation)


---------------------------------------------------------------------------------------------------



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

---------------------------------------------------------------------------------------------------


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

