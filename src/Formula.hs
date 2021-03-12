
module Formula
       (
         turnPositive
       , isPositive
       , mkClause
       , isEmptyClause
       , mkCnf
       , isEmptyCnf
       , hasEmptyClause
       , xorSat
       , orSat
       , andSat
       , implSat
       , equivSat
       ) where


import qualified Data.Set as Set
import qualified SAT.MiniSat as Sat

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



---------------------------------------------------------------------------------------------------


xorSat :: (Formula a, Formula b) => a -> b -> Bool
xorSat f1 f2 = Sat.satisfiable (convert f1 Sat.:++: convert f2)



andSat :: (Formula a, Formula b) => a -> b -> Bool
andSat f1 f2 = Sat.satisfiable (convert f1 Sat.:&&: convert f2)


orSat :: (Formula a, Formula b) => a -> b -> Bool
orSat f1 f2 = Sat.satisfiable (convert f1 Sat.:||: convert f2)


implSat :: (Formula a, Formula b) => a -> b -> Bool
implSat f1 f2 = Sat.satisfiable (convert f1 Sat.:->: convert f2)


equivSat :: (Formula a, Formula b) => a -> b -> Bool
equivSat f1 f2 = Sat.satisfiable (convert f1 Sat.:<->: convert f2)


