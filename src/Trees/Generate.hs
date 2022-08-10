module Trees.Generate (
 genSynTree,
) where

import Test.QuickCheck (choose, Gen, oneof, shuffle, suchThat, elements)
import Test.QuickCheck.Gen (vectorOf)

import Trees.Types (SynTree(..), Op(..))
import Trees.Helpers (collectLeaves, relabelShape, maxNodesForDepth, consecutiveNegations)

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
