module Formula where

import qualified Data.Set as Set
import Types

mkCnf :: [Clause] -> Cnf
mkCnf xs = Cnf $ Set.fromList xs

mkClause :: [Literal] -> Clause
mkClause xs = Clause $ Set.fromList xs
