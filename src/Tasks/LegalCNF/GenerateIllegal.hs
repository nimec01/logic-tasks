module Tasks.LegalCNF.GenerateIllegal (
  genIllegalSynTree,
) where

import Test.QuickCheck (choose, Gen, suchThat, elements, frequency)
import Test.QuickCheck.Gen (vectorOf, oneof)
import qualified Types as Setform
import Trees.Types (SynTree(..), BinOp(..), allBinaryOperators)
import Data.Set (toList, size, fromList)
import Trees.Helpers(relabelShape, transferLiteral, transferClause, collectLeaves, transferCnfToSyntree)
import Data.List((\\), sort)
import Tasks.LegalCNF.GenerateLegal (genClause, genLiteral, genClauseList)
import Auxiliary(listNoDuplicate)

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
            genLegalClauses (minClauseLength, maxClauseLength) usedLiterals firstSyntaxShape
        else do
            clauses <- choose (minClauseAmount, maxClauseAmount)
            genCNFWithOneIllegalClause (minClauseLength, maxClauseLength) usedLiterals (clauses - 1)

genCNFWithOneIllegalClause :: (Int,Int) -> [Char] -> Int -> Gen (SynTree BinOp Char)
genCNFWithOneIllegalClause (minClauseLength, maxClauseLength) usedLiterals ands
    | ands == 0 = illegalClauseTree (minClauseLength, maxClauseLength) usedLiterals
    | otherwise = do
        clauseList <- genClauseList (ands, ands) (minClauseLength, maxClauseLength) usedLiterals
        illegalTree <- illegalClauseTree (minClauseLength, maxClauseLength) usedLiterals
        let illLength = length (collectLeaves illegalTree)
        return (genTreeWithListAndIllegal (sort clauseList) (Just illegalTree) illLength)

genTreeWithListAndIllegal :: [Setform.Clause] -> Maybe (SynTree BinOp Char) -> Int -> SynTree BinOp Char
genTreeWithListAndIllegal [clause] Nothing _ = transferLiteral(transferClause (Leaf (toList (Setform.literalSet clause))))
genTreeWithListAndIllegal [clause] (Just illegalTree) illLength = if illLength <= size (Setform.literalSet clause) then Binary And illegalTree (genTreeWithListAndIllegal [clause] Nothing illLength) else Binary And (genTreeWithListAndIllegal [clause] Nothing illLength) illegalTree
genTreeWithListAndIllegal clauseList Nothing _ = transferCnfToSyntree (Setform.Cnf (fromList clauseList))
genTreeWithListAndIllegal (clause:clauseList) (Just illegalTree) illLength = if illLength <= size (Setform.literalSet clause)
                                                                             then Binary And illegalTree (genTreeWithListAndIllegal (clause:clauseList) Nothing illLength)
                                                                             else Binary And (transferLiteral(transferClause (Leaf (toList (Setform.literalSet clause))))) (genTreeWithListAndIllegal clauseList (Just illegalTree) illLength)
genTreeWithListAndIllegal _ _ _ = error "will not occured"

genLegalClauses :: (Int,Int) -> [Char] -> SynTree BinOp () -> Gen (SynTree BinOp Char)
genLegalClauses (minClauseLength, maxClauseLength) usedLiterals (Not a) = Not <$> genLegalClauses (minClauseLength, maxClauseLength) usedLiterals a
genLegalClauses (minClauseLength, maxClauseLength) usedLiterals (Binary oper a b) = do
    leftTree <- genLegalClauses (minClauseLength, maxClauseLength) usedLiterals a
    rightTree <- genLegalClauses (minClauseLength, maxClauseLength) usedLiterals b
    return (Binary oper leftTree rightTree)
genLegalClauses len usedLiterals (Leaf ()) = legalCluaseTree len usedLiterals


legalCluaseTree :: (Int,Int) -> [Char] -> Gen (SynTree BinOp Char)
legalCluaseTree (minClauseLength, maxClauseLength) usedLiterals = do
    clause <- genClause (minClauseLength, maxClauseLength) usedLiterals
    let clauseSynTree = transferLiteral(transferClause (Leaf (toList (Setform.literalSet clause))))
    return clauseSynTree

illegalClauseTree :: (Int,Int) -> [Char] -> Gen (SynTree BinOp Char)
illegalClauseTree (minClauseLength, maxClauseLength) usedLiterals = do
    len <- choose (max 2 minClauseLength, maxClauseLength)
    illegalSynTreeShape <- genIllegalClauseShape True (len - 1)
    leaves <- vectorOf len (genLiteral usedLiterals) `suchThat` listNoDuplicate
    return (transferLiteral (relabelShape illegalSynTreeShape (sort leaves)))

genIllegalShapeInSubTree :: Int -> (Int -> Gen (SynTree BinOp ())) -> BinOp -> Gen (SynTree BinOp ())
genIllegalShapeInSubTree opers illegalFunc oper = do
    opersIllegalSide <- choose (1, opers - 1)
    node <- elements [Binary oper, flip (Binary oper)]
    illegalSubTree <- illegalFunc opersIllegalSide
    return (node illegalSubTree (legalClauseShape (opers - 1 - opersIllegalSide)))


genIllegalClauseShape :: Bool -> Int -> Gen (SynTree BinOp ())
genIllegalClauseShape _ 0 = error "impossible"
genIllegalClauseShape ifFirstlayer ors = do
    ifUseError <- frequency [(1, return True), (ors - 1, return False)]
    if ifUseError
    then oneof [return (Not (legalClauseShape ors)), genIllegalOper legalClauseShape (if ifFirstlayer then [Equi, Impl] else [Equi, Impl, And]) ors]
    else genIllegalShapeInSubTree ors (genIllegalClauseShape False) Or

legalClauseShape :: Int -> SynTree BinOp ()
legalClauseShape = legalShape Or

genIllegalCNFShape :: Int -> Gen (SynTree BinOp ())
genIllegalCNFShape 0 = error "impossible"
genIllegalCNFShape 1 = oneof [return (Not (legalCNFShape 1)), genIllegalOper legalCNFShape (allBinaryOperators \\ [And, Or]) 1]
genIllegalCNFShape ands = do
    ifUseError <- frequency[(1, return True), (ands - 1, return False)]
    if ifUseError
    then oneof [return (Not (legalCNFShape ands)), genIllegalOper legalCNFShape (allBinaryOperators \\ [And]) ands]
    else genIllegalShapeInSubTree ands genIllegalCNFShape And

legalCNFShape :: Int -> SynTree BinOp ()
legalCNFShape = legalShape And

genIllegalOper :: (Int -> SynTree BinOp ()) -> [BinOp] -> Int -> Gen (SynTree BinOp ())
genIllegalOper recF opers restOpers =
    do
        errorOper <- elements opers
        leftOpers <- choose (0, restOpers - 1)
        return (Binary errorOper (recF leftOpers) (recF (restOpers - 1 - leftOpers)))

legalShape :: BinOp -> Int -> SynTree BinOp ()
legalShape oper opers = foldr (Binary oper . Leaf) (Leaf ()) (replicate opers ())
