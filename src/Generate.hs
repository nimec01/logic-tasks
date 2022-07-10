module Generate(
 rangeDepthForNodes,
 genSynTree,
 maxLeavesForNodes,
 maxNodesForDepth,
 genSynTreeSubtreeExc,
 judgeDupTree
) where

import Types (SynTree(..),collectLeaves, relabelShape, allSubtre)
import Test.QuickCheck (choose, oneof, Gen, shuffle, suchThat)
import Data.List (nub)
import Data.Set (size)

rangeDepthForNodes :: Integer -> (Integer, Integer)
rangeDepthForNodes nodes = (minDepth, maxDepth)
  where
    minDepth = head [ depth | depth <- [1..], maxNodesForDepth depth >= nodes ]
    maxDepth = nodes

maxNodesForDepth :: Integer -> Integer
maxNodesForDepth depth = 2 ^ depth - 1

maxLeavesForNodes :: Integer -> Integer
maxLeavesForNodes nodes = (nodes + 1) `div` 2

chooseList :: Bool -> [SynTree c -> SynTree c -> SynTree c]
chooseList useImplEqui = if useImplEqui
        then [And, Or, Impl, Equi]
        else [And, Or]

randomuse :: [c] -> [c] -> Integer -> Gen [c]
randomuse lits minuse len = let
    uselist = minuse ++ cycle ( reverse lits)
    in
      shuffle (take (fromIntegral len) uselist)

genSynTree :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Gen (SynTree Char)
genSynTree (minnode, maxnode) maxdepth lits minuse useImplEqui =
    let minuseNode = minuse * 2 - 1
        finalMinuse = max minnode minuseNode
        maxnodeWithdepth = min maxnode $ maxNodesForDepth maxdepth
    in  do
    nodenum <- choose (finalMinuse , maxnodeWithdepth)
    sample <- syntaxShape nodenum maxdepth useImplEqui `suchThat` \synTree -> fromIntegral (length (collectLeaves synTree)) >= minuse
    usedlist <- randomuse lits (take (fromIntegral minuse) lits) $ fromIntegral $ length $ collectLeaves sample
    return (relabelShape sample usedlist )

syntaxShape :: Integer -> Integer -> Bool -> Gen (SynTree ())
syntaxShape nodenum maxdepth useImplEqui
    | nodenum == 1 = positiveLiteral
    | nodenum == 2 = negativeLiteral
    | maxNodesForDepth (maxdepth - 1) < nodenum - 1 = oneof binaryOper
    | otherwise = oneof $ negativeForm : binaryOper
    where
        binaryOper = map (binaryOperator nodenum maxdepth useImplEqui) $ chooseList useImplEqui
        negativeForm = negativeFormula nodenum maxdepth useImplEqui

binaryOperator :: Integer -> Integer -> Bool -> (SynTree () -> SynTree () -> SynTree ()) -> Gen (SynTree ())
binaryOperator nodenum maxdepth useImplEqui operator =
    let minNodesPerSide = max 1 (restNodes - maxNodesForDepth newMaxDepth)
        restNodes = nodenum - 1
        newMaxDepth = maxdepth - 1
    in  do
        leftNodenum <- choose (minNodesPerSide , restNodes - minNodesPerSide)
        leftTre <- syntaxShape leftNodenum newMaxDepth useImplEqui
        rightTre <- syntaxShape (restNodes - leftNodenum ) newMaxDepth useImplEqui
        return (operator leftTre rightTre)

negativeFormula :: Integer -> Integer -> Bool -> Gen (SynTree ())
negativeFormula nodenum maxdepth useImplEqui =
  let restNodes = nodenum - 1
      newMaxDepth = maxdepth - 1
  in  do
      e <- syntaxShape restNodes newMaxDepth useImplEqui
      return (Not e)

negativeLiteral ::  Gen (SynTree ())
negativeLiteral = Not <$> positiveLiteral

positiveLiteral :: Gen (SynTree ())
positiveLiteral = return (Leaf ())

--------------------------------------------------------------------------------------------------------------
judgeDupTree :: Eq c => SynTree c -> Bool
judgeDupTree synTree = let treeList = collectLeaves synTree
    in
        treeList == nub treeList
-- generate subtree exercise
genSynTreeSubtreeExc :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Bool -> Integer -> Gen (SynTree Char)
genSynTreeSubtreeExc (minnode, maxnode) maxdepth lits minuse useImplEqui useDupTree subtreeNum = genSynTree (minnode, maxnode) maxdepth lits minuse useImplEqui `suchThat` \synTree -> (judgeDupTree synTree || useDupTree) && size (allSubtre synTree) == fromIntegral subtreeNum