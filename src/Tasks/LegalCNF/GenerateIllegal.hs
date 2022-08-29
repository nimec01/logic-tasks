module Tasks.LegalCNF.GenerateIllegal (
  genIllegalSynTree,
) where

import Test.QuickCheck (choose, Gen, elements, frequency)
import Test.QuickCheck.Gen (oneof)
import qualified Types as Setform
import Trees.Types (SynTree(..), BinOp(..), allBinaryOperators)
import Data.Set (toList, size)
import Trees.Helpers (relabelShape, transferLiteral, clauseToSynTree, collectLeaves)
import Data.List ((\\))
import Tasks.LegalCNF.GenerateLegal (genClause, genCnf)
import Types (Clause(Clause))
import Control.Monad (join)

genIllegalSynTree :: (Int,Int) -> (Int,Int) -> [Char] -> Gen (SynTree BinOp Char)
genIllegalSynTree (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals = do
    ifUseError <- elements [True,False]
    let ifUseError'
          | maxClauseAmount == 1 = False
          | maxClauseLength == 1 = True
          | otherwise = ifUseError
     in if ifUseError'
        then do
            clauses <- choose (max 2 minClauseAmount, maxClauseAmount)
            firstSyntaxShape <- genIllegalCNFShape (clauses - 1)
            clauseList <- toList . Setform.clauseSet <$> genCnf (clauses, clauses) (minClauseLength, maxClauseLength) usedLiterals
            return (genIllegalCNF firstSyntaxShape clauseList)
        else do
            clauses <- choose (minClauseAmount, maxClauseAmount)
            genCNFWithOneIllegalClause (minClauseLength, maxClauseLength) usedLiterals (clauses - 1)

genCNFWithOneIllegalClause :: (Int,Int) -> [Char] -> Int -> Gen (SynTree BinOp Char)
genCNFWithOneIllegalClause (minClauseLength, maxClauseLength) usedLiterals ands
    | ands == 0 = illegalClauseTree (minClauseLength, maxClauseLength) usedLiterals
    | otherwise = do
        clauseList <- toList . Setform.clauseSet <$> genCnf (ands, ands) (minClauseLength, maxClauseLength) usedLiterals
        illegalTree <- illegalClauseTree (minClauseLength, maxClauseLength) usedLiterals
        let illLength = length (collectLeaves illegalTree)
            (first, second) = span (\(Clause clause) -> illLength >= size clause) clauseList
            headTrees = map clauseToSynTree first
            tailTrees = map clauseToSynTree second
        return (foldr1 (Binary And) (headTrees ++ (illegalTree : tailTrees)))

genIllegalCNF :: SynTree BinOp () -> [Setform.Clause] -> SynTree BinOp Char
genIllegalCNF treeShape = join . relabelShape treeShape . map clauseToSynTree

illegalClauseTree :: (Int,Int) -> [Char] -> Gen (SynTree BinOp Char)
illegalClauseTree (minClauseLength, maxClauseLength) usedLiterals = do
    len <- choose (max 2 minClauseLength, maxClauseLength)
    illegalSynTreeShape <- genIllegalClauseShape True (len - 1)
    leaves <- toList . Setform.literalSet <$> genClause (len,len) usedLiterals
    return (transferLiteral (relabelShape illegalSynTreeShape leaves))

genIllegalShapeInSubTree :: Int -> (Int -> Gen (SynTree BinOp ())) -> BinOp -> Gen (SynTree BinOp ())
genIllegalShapeInSubTree opers illegalFunc oper = do
    opersIllegalSide <- choose (1, opers - 1)
    node <- elements [Binary oper, flip (Binary oper)]
    illegalSubTree <- illegalFunc opersIllegalSide
    return (node illegalSubTree (legalShape Or (opers - 1 - opersIllegalSide)))


genIllegalClauseShape :: Bool -> Int -> Gen (SynTree BinOp ())
genIllegalClauseShape _ 0 = error "impossible"
genIllegalClauseShape ifFirstLayer ors = do
    ifUseError <- frequency [(1, return True), (ors - 1, return False)]
    if ifUseError
    then oneof [return (Not (legalShape Or ors)), genIllegalOper (legalShape Or) (if ifFirstLayer then [Equi, Impl] else [Equi, Impl, And]) ors]
    else genIllegalShapeInSubTree ors (genIllegalClauseShape False) Or

genIllegalCNFShape :: Int -> Gen (SynTree BinOp ())
genIllegalCNFShape 0 = error "impossible"
genIllegalCNFShape 1 = oneof [return (Not (legalShape And 1)), genIllegalOper (legalShape And) (allBinaryOperators \\ [And, Or]) 1]
genIllegalCNFShape ands = do
    ifUseError <- frequency[(1, return True), (ands - 1, return False)]
    if ifUseError
    then oneof [return (Not (legalShape And ands)), genIllegalOper (legalShape And) (allBinaryOperators \\ [And]) ands]
    else genIllegalShapeInSubTree ands genIllegalCNFShape And

genIllegalOper :: (Int -> SynTree BinOp ()) -> [BinOp] -> Int -> Gen (SynTree BinOp ())
genIllegalOper recF opers restOpers =
    do
        errorOper <- elements opers
        leftOpers <- choose (0, restOpers - 1)
        return (Binary errorOper (recF leftOpers) (recF (restOpers - 1 - leftOpers)))

legalShape :: BinOp -> Int -> SynTree BinOp ()
legalShape oper opers = foldr (Binary oper . Leaf) (Leaf ()) (replicate opers ())
