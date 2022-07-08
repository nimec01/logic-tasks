module Generate(
 rangeDepthForNodes,
 genSynTree,
 maxLeavesForNodes,
 maxNodesForDepth,
) where

import Types (SynTree(..),collectLeaves, relabelShape)
import Test.QuickCheck (choose, oneof, Gen, shuffle, suchThat,chooseInt)

rangeDepthForNodes :: Int -> (Int, Int)
rangeDepthForNodes nodes = (minDepth, maxDepth)
  where
    minDepth = head [ depth | depth <- [1..], maxNodesForDepth depth >= nodes ]
    maxDepth = nodes

maxNodesForDepth :: Int -> Int
maxNodesForDepth depth = 2 ^ depth - 1

maxLeavesForNodes :: Int -> Int
maxLeavesForNodes nodes = (nodes + 1) `div` 2

chooseList :: Bool -> [SynTree c -> SynTree c -> SynTree c]
chooseList addoper = if addoper
    then [And, Or, Impl, Equi]
    else [And, Or]

randomuse :: [c] -> [c] -> Int -> Gen [c]
randomuse lits minuse len = let
    uselist = minuse ++ cycle lits
    in
      shuffle (take len uselist)

genSynTree :: (Int, Int) -> Int -> String -> Int -> Bool -> Maybe (Gen (SynTree Char))
genSynTree (minnode, maxnode) maxdepth lits minuse addoper
    | maxdepth <= 0 || maxnode <= 0 || null lits || length lits < minuse || maxnode < minnode || fst (rangeDepthForNodes minnode) > maxdepth || maxLeavesForNodes maxnode < minuse || 2 ^ (maxdepth - 1) < minuse = Nothing
    | otherwise =  Just $ do
        toUse <- take minuse <$> shuffle lits
        syntaxTree (a,maxnode) maxdepth lits toUse addoper
      where
        a=max 0 minnode

syntaxTree :: (Int, Int) -> Int -> [c] -> [c] -> Bool -> Gen (SynTree c)
syntaxTree (minnode, maxnode) maxdepth lits minuse addoper = 
    let minuseNode = (length minuse *2 - 1)
        finalMinuse = max minnode minuseNode
        lenMinuse = length minuse
        maxnodeWithdepth = min maxnode $ maxNodesForDepth maxdepth
    in  do
    nodenum <- chooseInt (finalMinuse , maxnodeWithdepth)
    sample <- syntaxShape nodenum maxdepth  addoper `suchThat` (\synTree -> length ( collectLeaves synTree) >= lenMinuse)
    usedlist <- randomuse lits minuse (length $ collectLeaves sample)
    return (relabelShape sample usedlist )

syntaxShape :: Int -> Int -> Bool -> Gen (SynTree ())
syntaxShape nodenum maxdepth addoper
    | nodenum == 1 = positiveLiteral
    | nodenum == 2 = negativeLiteral
    | maxNodesForDepth (maxdepth - 1) < nodenum - 1 = oneof binaryOper
    | otherwise = oneof $ negativeForm : binaryOper
    where
        binaryOper = map (binaryOperator nodenum maxdepth addoper) $ chooseList addoper
        negativeForm = negativeFormula nodenum maxdepth addoper

binaryOperator :: Int -> Int -> Bool -> (SynTree () -> SynTree () -> SynTree ()) -> Gen (SynTree ())
binaryOperator nodenum maxdepth addoper operator =
    let minNodesPerSide = max 1 (restNodes - maxNodesForDepth newMaxDepth)
        restNodes = nodenum - 1
        newMaxDepth = maxdepth - 1
    in  do
        leftNodenum <- choose (minNodesPerSide , restNodes - minNodesPerSide)
        leftTre <- syntaxShape leftNodenum newMaxDepth addoper
        rightTre <- syntaxShape (restNodes - leftNodenum ) newMaxDepth addoper
        return (operator leftTre rightTre)

negativeFormula :: Int -> Int -> Bool -> Gen (SynTree ())
negativeFormula nodenum maxdepth addoper =
  let restNodes = nodenum - 1
      newMaxDepth = maxdepth - 1
  in  do
      e <- syntaxShape restNodes newMaxDepth addoper
      return (Not e)

negativeLiteral ::  Gen (SynTree ())
negativeLiteral = Not <$> positiveLiteral

positiveLiteral :: Gen (SynTree ())
positiveLiteral = return (Leaf ())