
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


logOpSat :: (Formula a, Formula b)
         => (Sat.Formula Char -> Sat.Formula Char -> Sat.Formula Char)
         -> a
         -> b
         -> Bool
logOpSat op f1 f2 = Sat.satisfiable (op (convert f1) (convert f2))



xorSat :: (Formula a, Formula b) => a -> b -> Bool
xorSat = logOpSat (Sat.:++:)


andSat :: (Formula a, Formula b) => a -> b -> Bool
andSat = logOpSat (Sat.:&&:)


orSat :: (Formula a, Formula b) => a -> b -> Bool
orSat = logOpSat (Sat.:||:)


implSat :: (Formula a, Formula b) => a -> b -> Bool
implSat = logOpSat (Sat.:->:)


equivSat :: (Formula a, Formula b) => a -> b -> Bool
equivSat = logOpSat (Sat.:<->:)
