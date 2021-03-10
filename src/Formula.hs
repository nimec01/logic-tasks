module Formula
       (
         mkClause
       , mkCnf
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


---------------------------------------------------------------------------------------------------


mkCnf :: [Clause] -> Cnf
mkCnf xs = Cnf $ Set.fromList xs