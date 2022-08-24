module Tasks.LegalCNF.GenerateLegal (
  genSynTreeWithCnf,
  genClause,
  genLiteral,
  genClauseList
) where

import Test.QuickCheck (choose, Gen, suchThat, elements)
import Test.QuickCheck.Gen (vectorOf)
import qualified Types as Setform
import Trees.Types (SynTree(..), BinOp(..))
import Data.Set (fromList)
import Trees.Helpers(transferCnfToSyntree,)
import Auxiliary (listNoDuplicate)

genLiteral :: [Char] -> Gen Setform.Literal
genLiteral [] = error "Can not construct Literal from empty list."
genLiteral lits = do
   rChar <- elements lits
   elements [Setform.Literal rChar, Setform.Not rChar]


genClause :: (Int,Int) -> [Char] -> Gen Setform.Clause
genClause (minClauseLength, maxClauseLength) usedLiterals = do
    literals <- choose (minClauseLength, maxClauseLength)
    clause <- vectorOf literals (genLiteral usedLiterals) `suchThat` \clause -> listNoDuplicate clause
    return (Setform.Clause (fromList clause))

genClauseList :: (Int,Int) -> (Int,Int) -> [Char] -> Gen [Setform.Clause]
genClauseList (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals = do
  clauses <- choose (minClauseAmount, maxClauseAmount)
  vectorOf clauses (genClause (minClauseLength, maxClauseLength) usedLiterals) `suchThat` \cnf -> listNoDuplicate cnf

genSynTreeWithCnf :: (Int,Int) -> (Int,Int) -> [Char] -> Gen (SynTree BinOp Char)
genSynTreeWithCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals = do
    clauses <- choose (minClauseAmount, maxClauseAmount)
    cnf <- vectorOf clauses (genClause (minClauseLength, maxClauseLength) usedLiterals) `suchThat` \cnf -> listNoDuplicate cnf
    return (transferCnfToSyntree (Setform.Cnf (fromList cnf)))
