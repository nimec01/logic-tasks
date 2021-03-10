module Formula
       (
         mkClause
       , isEmptyClause
       , mkCnf
       , isEmptyCnf
       , hasEmptyClause
       , turnPositive
       , isPositive
       ) where


import qualified Data.Set as Set

import Types


---------------------------------------------------------------------------------------------------

turnPositive :: Literal -> Literal
turnPositive = head . atomics


isPositive :: Literal -> Bool
isPositive (Not _) = False
isPositive _ = True

---------------------------------------------------------------------------------------------------

mkClause :: [Literal] -> Clause
mkClause xs = Clause $ Set.fromList xs


isEmptyClause :: Clause -> Bool
isEmptyClause (Clause set) = Set.null set

---------------------------------------------------------------------------------------------------


mkCnf :: [Clause] -> Cnf
mkCnf xs = Cnf $ Set.fromList xs


isEmptyCnf :: Cnf -> Bool
isEmptyCnf (Cnf set) = Set.null set


hasEmptyClause :: Cnf -> Bool
hasEmptyClause (Cnf set) = Clause Set.empty `Set.member` set
