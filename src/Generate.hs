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

import Types (SynTree(..), Op(..), collectLeaves, relabelShape, allNotLeafSubTrees, getNotLeafSubTrees)

minDepthForNodes :: Integer -> Integer
minDepthForNodes nodes = ceiling (logBase 2 (fromIntegral (nodes + 1) :: Float))

maxNodesForDepth :: Integer -> Integer
maxNodesForDepth depth = 2 ^ depth - 1

maxLeavesForNodes :: Integer -> Integer
maxLeavesForNodes nodes = (nodes + 1) `div` 2

chooseList :: Bool -> [Op]
chooseList useImplEqui = if useImplEqui
        then [And, Or, Impl, Equi]
        else [And, Or]

randomList :: [c] -> [c] -> Integer -> Gen [c]
randomList availableLetters atLeastOccurring len = let
    restLength = fromIntegral len - length atLeastOccurring
    in do
        randomRest <- vectorOf restLength (elements availableLetters)
        shuffle (atLeastOccurring ++ randomRest)

consecutiveNegations :: SynTree Op a -> Integer
consecutiveNegations (Binary _ a b) = max (consecutiveNegations a) (consecutiveNegations b)
consecutiveNegations (Unary Not a) = max (consecutiveNegations a) (1 + continueNot a)
consecutiveNegations (Leaf _)  = 0
consecutiveNegations _ = error "All cases handled!"

continueNot :: SynTree Op a -> Integer
continueNot (Unary Not a) = 1+ continueNot a
continueNot _ = 0

genSynTree :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Integer -> Gen (SynTree Op Char)
genSynTree (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui maxConsecutiveNegations =
    if maxConsecutiveNegations /= 0
        then do
        nodes <- choose (minNodes, maxNodes)
        sample <- syntaxShape nodes maxDepth useImplEqui True `suchThat` \synTree -> (fromIntegral (length (collectLeaves synTree)) >= atLeastOccurring) && consecutiveNegations synTree <= maxConsecutiveNegations
        usedList <- randomList availableLetters (take (fromIntegral atLeastOccurring) availableLetters) $ fromIntegral $ length $ collectLeaves sample
        return (relabelShape sample usedList )
        else do
        nodes <- choose (minNodes, maxNodes) `suchThat` odd
        sample <- syntaxShape nodes maxDepth useImplEqui False `suchThat` \synTree -> fromIntegral (length (collectLeaves synTree)) >= atLeastOccurring
        usedList <- randomList availableLetters (take (fromIntegral atLeastOccurring) availableLetters) $ fromIntegral $ length $ collectLeaves sample
        return (relabelShape sample usedList )

syntaxShape :: Integer -> Integer -> Bool -> Bool -> Gen (SynTree Op ())
syntaxShape nodes maxDepth useImplEqui allowNegation
    | nodes == 1 = positiveLiteral
    | nodes == 2 = negativeLiteral
    | not allowNegation = oneof binaryOper
    | maxNodesForDepth (maxDepth - 1) < nodes - 1 = oneof binaryOper
    | otherwise = oneof $ negativeForm : binaryOper
    where
        binaryOper = map (binaryOperator nodes maxDepth useImplEqui allowNegation . Binary) $ chooseList useImplEqui
        negativeForm = negativeFormula nodes maxDepth useImplEqui

binaryOperator :: Integer -> Integer -> Bool -> Bool -> (SynTree Op () -> SynTree Op () -> SynTree Op ()) -> Gen (SynTree Op ())
binaryOperator nodes maxDepth useImplEqui allowNegation operator =
    let minNodesPerSide = max 1 (restNodes - maxNodesForDepth newMaxDepth)
        restNodes = nodes - 1
        newMaxDepth = maxDepth - 1
    in  do
        leftNodes <- choose (minNodesPerSide , restNodes - minNodesPerSide) `suchThat` \leftNodes -> allowNegation || odd leftNodes
        leftTree <- syntaxShape leftNodes newMaxDepth useImplEqui allowNegation
        rightTree <- syntaxShape (restNodes - leftNodes ) newMaxDepth useImplEqui allowNegation
        return (operator leftTree rightTree)

negativeFormula :: Integer -> Integer -> Bool -> Gen (SynTree Op ())
negativeFormula nodes maxDepth useImplEqui =
    let restNodes = nodes - 1
        newMaxDepth = maxDepth - 1
    in  do
        e <- syntaxShape restNodes newMaxDepth useImplEqui True
        return (Unary Not e)

negativeLiteral ::  Gen (SynTree Op ())
negativeLiteral = Unary Not <$> positiveLiteral

positiveLiteral :: Gen (SynTree o ())
positiveLiteral = return (Leaf ())

--------------------------------------------------------------------------------------------------------------
noSameSubTree :: (Eq o, Ord o, Ord c) => SynTree o c -> Bool
noSameSubTree synTree = let treeList = getNotLeafSubTrees synTree
    in
        treeList == nubOrd treeList
-- generate subTree exercise
genSynTreeSubTreeExc :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Bool -> Integer -> Integer -> Gen (SynTree Op Char)
genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui allowDupelTree maxConsecutiveNegations minSubTrees =
    let
        syntaxTree = genSynTree (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui maxConsecutiveNegations
    in
        syntaxTree `suchThat` \synTree -> (allowDupelTree || noSameSubTree synTree) && fromIntegral (size (allNotLeafSubTrees synTree)) >= minSubTrees
-------------------------------------------------------------------------------------------------------------------
similarTree :: Eq o => SynTree o c -> SynTree o c -> Bool
similarTree (Binary o1 a b) (Binary o2 c d) | o1 == o2 = similarTree a c && similarTree b d
similarTree (Unary o1 a) (Unary o2 c) | o1 == o2 = similarTree a c
similarTree (Leaf _) (Leaf _) = True
similarTree _ _ = False

similarExist :: Eq o => [SynTree o c] -> Bool
similarExist trees = length (nubBy similarTree trees) /= length trees
