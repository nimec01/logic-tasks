
module Tasks.LegalCNF.GenerateLegal (
    genClause,
    genCnf
    ) where


import qualified Formula.Types as SetFormula

import Data.Set (fromList)
import Test.QuickCheck (Gen, choose, elements, suchThat)
import Test.QuickCheck.Gen (vectorOf)

import Auxiliary (listNoDuplicate)




genLiteral :: [Char] -> Gen SetFormula.Literal
genLiteral lits = do
   rChar <- elements lits
   elements [SetFormula.Literal rChar, SetFormula.Not rChar]



genClause :: (Int,Int) -> [Char] -> Gen SetFormula.Clause
genClause (minClauseLength, maxClauseLength) usedLiterals = do
    literals <- choose (minClauseLength, maxClauseLength)
    clause <- vectorOf literals (genLiteral usedLiterals) `suchThat` listNoDuplicate
    return (SetFormula.Clause (fromList clause))



genCnf :: (Int,Int) -> (Int,Int) -> [Char] -> Gen SetFormula.Cnf
genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals = do
  clauses <- choose (minClauseAmount, maxClauseAmount)
  cnf <- vectorOf clauses (genClause (minClauseLength, maxClauseLength) usedLiterals) `suchThat` listNoDuplicate
  return (SetFormula.Cnf (fromList cnf))
