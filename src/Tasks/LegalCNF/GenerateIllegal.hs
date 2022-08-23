module Tasks.LegalCNF.GenerateIllegal (
  genIllegalSynTree,
) where

import Test.QuickCheck (choose, Gen, suchThat, elements, frequency)
import Test.QuickCheck.Gen (vectorOf, oneof)
import qualified Types as Setform
import Trees.Types (SynTree(..), BinOp(..), allBinaryOperators)
import Data.Set (toList)
import Trees.Helpers(relabelShape, transferLiteral, transferClause, collectLeaves)
import Data.List((\\))
import Tasks.LegalCNF.GenerateLegal (genClause, genLiteral)
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
            firstSyntaxShape <- genLegalCNFShape (clauses - 1)
            genIllegalClauses (minClauseLength, maxClauseLength) usedLiterals firstSyntaxShape

genIllegalClauses :: (Int,Int) -> [Char] -> SynTree BinOp () -> Gen (SynTree BinOp Char)
genIllegalClauses (minClauseLength, maxClauseLength) usedLiterals (Binary oper a b) = do
    let leftNodes = length (collectLeaves a)
        rightNodes = length (collectLeaves b)
    (illegalShape, legalShape) <- frequency [(leftNodes, return (a,b)), (rightNodes, return (b,a))]
    illegalSubTree <- genIllegalClauses (minClauseLength, maxClauseLength) usedLiterals illegalShape
    legalSubTree <- genLegalClauses (minClauseLength, maxClauseLength) usedLiterals legalShape
    node <- elements [Binary oper, flip (Binary oper)]
    return (node illegalSubTree legalSubTree)
genIllegalClauses len usedLiterals (Leaf ()) = illegalClauseTree len usedLiterals
genIllegalClauses _ _ _ = error "can not match rest thing"

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
    illegalSynTreeShape <- illegalClauseShape True (len - 1)
    leaves <- vectorOf len (genLiteral usedLiterals) `suchThat` listNoDuplicate
    return (transferLiteral (relabelShape illegalSynTreeShape leaves))

illegalClauseShape :: Bool -> Int -> Gen (SynTree BinOp ())
illegalClauseShape ifFirstlayer ors = do
    ifUseError <- frequency [(1, return True), (ors - 1, return False)]
    if ifUseError
    then oneof [Not <$> legalClauseShape ors, genIllegalOper legalClauseShape (if ifFirstlayer then [Equi, Impl] else [Equi, Impl, And]) ors]
    else do
        orsIllegalSide <- choose (1, ors - 1)
        illegalSubTree <- illegalClauseShape False orsIllegalSide
        legalSubTree <- legalClauseShape (ors - 1 - orsIllegalSide)
        node <- elements [Binary Or, flip (Binary Or)]
        return (node illegalSubTree legalSubTree)

legalClauseShape :: Int -> Gen (SynTree BinOp ())
legalClauseShape 0 = return (Leaf ())
legalClauseShape ors =  Binary Or (Leaf ()) <$> legalClauseShape (ors - 1)


genIllegalCNFShape :: Int -> Gen (SynTree BinOp ())
genIllegalCNFShape 1 = oneof [Not <$> genLegalCNFShape 1, genIllegalOper genLegalCNFShape (allBinaryOperators \\ [And, Or]) 1]
genIllegalCNFShape ands = do
    ifUseError <- frequency[(1, return True), (ands - 1, return False)]
    if ifUseError
    then oneof [Not <$> genLegalCNFShape ands, genIllegalOper genLegalCNFShape (allBinaryOperators \\ [And]) ands]
    else do
        andsIllegalSide <- choose (1, ands - 1)
        illegalSubTree <- genIllegalCNFShape andsIllegalSide
        legalSubTree <- genLegalCNFShape (ands - 1 - andsIllegalSide)
        node <- elements [Binary And, flip (Binary And)]
        return (node illegalSubTree legalSubTree)

genLegalCNFShape :: Int -> Gen (SynTree BinOp ())
genLegalCNFShape 0 = return (Leaf ())
genLegalCNFShape ands = Binary And (Leaf ()) <$> genLegalCNFShape (ands - 1)

genIllegalOper :: (Int -> Gen (SynTree BinOp ())) -> [BinOp] -> Int -> Gen (SynTree BinOp ())
genIllegalOper recF opers restOpers =
    do
        errorOper <- elements opers
        leftOpers <- choose (0, restOpers - 1)
        leftSubTree <- recF leftOpers
        rightSubTree <- recF (restOpers - 1 - leftOpers)
        return (Binary errorOper leftSubTree rightSubTree)
