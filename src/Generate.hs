module Generate(
 minDepthForNodes,
 genSynTree,
 maxLeavesForNodes,
 maxNodesForDepth,
 genSynTreeSubTreeExc,
 noSameSubTree,
 similarExist,
) where

import Test.QuickCheck (choose, Gen, oneof, shuffle, suchThat, elements)
import Data.List.Extra (nubOrd, nubBy)
import Data.Set (size)
import Test.QuickCheck.Gen (vectorOf)

import Types (SynTree(..), collectLeaves, relabelShape, allNotLeafSubTrees, getNotLeafSubTrees)

minDepthForNodes :: Integer -> Integer
minDepthForNodes nodes = ceiling (logBase 2 (fromIntegral (nodes + 1) :: Float))

maxNodesForDepth :: Integer -> Integer
maxNodesForDepth depth = 2 ^ depth - 1

maxLeavesForNodes :: Integer -> Integer
maxLeavesForNodes nodes = (nodes + 1) `div` 2

chooseList :: Bool -> [SynTree c -> SynTree c -> SynTree c]
chooseList useImplEqui = if useImplEqui
        then [And, Or, Impl, Equi]
        else [And, Or]

randomList :: [c] -> [c] -> Integer -> Gen [c]
randomList availableLetters atLeastOccurring len = let
    restLength = fromIntegral len - length atLeastOccurring
    in do
        randomRest <- vectorOf restLength (elements availableLetters)
        shuffle (atLeastOccurring ++ randomRest)

consecutiveNegations :: SynTree a -> Int
consecutiveNegations (And a b) = max (consecutiveNegations a) (consecutiveNegations b)
consecutiveNegations (Or a b) = max (consecutiveNegations a) (consecutiveNegations b)
consecutiveNegations (Impl a b) = max (consecutiveNegations a) (consecutiveNegations b)
consecutiveNegations (Equi a b) = max (consecutiveNegations a) (consecutiveNegations b)
consecutiveNegations (Not a) = max (consecutiveNegations a) (1 + continueNot a)
consecutiveNegations (Leaf _)  = 0

continueNot :: SynTree a -> Int
continueNot (Not a) = 1+ continueNot a
continueNot _ = 0

genSynTree :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Integer -> Gen (SynTree Char)
genSynTree (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui maxConsecutiveNegations =
    if maxConsecutiveNegations /= 0
        then do
        nodes <- choose (minNodes, maxNodes)
        sample <- syntaxShape nodes maxDepth useImplEqui True `suchThat` \synTree -> (fromIntegral (length (collectLeaves synTree)) >= atLeastOccurring) && consecutiveNegations synTree <= fromIntegral maxConsecutiveNegations
        usedList <- randomList availableLetters (take (fromIntegral atLeastOccurring) availableLetters) $ fromIntegral $ length $ collectLeaves sample
        return (relabelShape sample usedList )
        else do
        nodes <- choose (minNodes, maxNodes) `suchThat` odd
        sample <- syntaxShape nodes maxDepth useImplEqui False `suchThat` \synTree -> fromIntegral (length (collectLeaves synTree)) >= atLeastOccurring
        usedList <- randomList availableLetters (take (fromIntegral atLeastOccurring) availableLetters) $ fromIntegral $ length $ collectLeaves sample
        return (relabelShape sample usedList )

syntaxShape :: Integer -> Integer -> Bool -> Bool -> Gen (SynTree ())
syntaxShape nodes maxDepth useImplEqui allowNegation
    | nodes == 1 = positiveLiteral
    | nodes == 2 = negativeLiteral
    | not allowNegation = oneof binaryOper
    | maxNodesForDepth (maxDepth - 1) < nodes - 1 = oneof binaryOper
    | otherwise = oneof $ negativeForm : binaryOper
    where
        binaryOper = map (binaryOperator nodes maxDepth useImplEqui allowNegation) $ chooseList useImplEqui
        negativeForm = negativeFormula nodes maxDepth useImplEqui

binaryOperator :: Integer -> Integer -> Bool -> Bool -> (SynTree () -> SynTree () -> SynTree ()) -> Gen (SynTree ())
binaryOperator nodes maxDepth useImplEqui allowNegation operator =
    let minNodesPerSide = max 1 (restNodes - maxNodesForDepth newMaxDepth)
        restNodes = nodes - 1
        newMaxDepth = maxDepth - 1
    in  do
        leftNodes <- choose (minNodesPerSide , restNodes - minNodesPerSide) `suchThat` \leftNodes -> allowNegation || odd leftNodes
        leftTree <- syntaxShape leftNodes newMaxDepth useImplEqui allowNegation
        rightTree <- syntaxShape (restNodes - leftNodes ) newMaxDepth useImplEqui allowNegation
        return (operator leftTree rightTree)

negativeFormula :: Integer -> Integer -> Bool -> Gen (SynTree ())
negativeFormula nodes maxDepth useImplEqui =
    let restNodes = nodes - 1
        newMaxDepth = maxDepth - 1
    in  do
        e <- syntaxShape restNodes newMaxDepth useImplEqui True
        return (Not e)

negativeLiteral ::  Gen (SynTree ())
negativeLiteral = Not <$> positiveLiteral

positiveLiteral :: Gen (SynTree ())
positiveLiteral = return (Leaf ())

--------------------------------------------------------------------------------------------------------------
noSameSubTree :: Ord c => SynTree c -> Bool
noSameSubTree synTree = let treeList = getNotLeafSubTrees synTree
    in
        treeList == nubOrd treeList
-- generate subTree exercise
genSynTreeSubTreeExc :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Bool -> Integer -> Integer -> Gen (SynTree Char)
genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui allowDupelTree maxConsecutiveNegations minSubTrees =
    let
        syntaxTree = genSynTree (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui maxConsecutiveNegations
    in
        syntaxTree `suchThat` \synTree -> (allowDupelTree || noSameSubTree synTree) && fromIntegral (size (allNotLeafSubTrees synTree)) >= minSubTrees
-------------------------------------------------------------------------------------------------------------------
similarTree :: SynTree Char -> SynTree Char -> Bool
similarTree (And a b) (And c d) = similarTree a c && similarTree b d
similarTree (Or a b) (Or c d) = similarTree a c && similarTree b d
similarTree (Impl a b) (Impl c d) = similarTree a c && similarTree b d
similarTree (Equi a b) (Equi c d) = similarTree a c && similarTree b d
similarTree (Not a) (Not c) = similarTree a c
similarTree (Leaf _) (Leaf _) = True
similarTree _ _ = False

similarExist :: [SynTree Char] -> Bool
similarExist trees = length (nubBy similarTree trees) /= length trees
