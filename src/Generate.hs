module Generate(
 rangeDepthForNodes,
 genSynTree,
 maxLeavesForNodes,
 maxNodesForDepth,
) where

import Types (SynTree(..))
import Test.QuickCheck (choose, elements, oneof, Gen, frequency, shuffle)
import Data.List ((\\))
import Data.Maybe(listToMaybe)


rangeDepthForNodes :: Int -> (Int, Int)
rangeDepthForNodes nodes = (minDepth, maxDepth)
  where
    minDepth = head [ depth | depth <- [1..], maxNodesForDepth depth >= nodes ]
    maxDepth = nodes

maxNodesForDepth :: Int -> Int
maxNodesForDepth depth = 2 ^ depth - 1

maxLeavesForNodes :: Int -> Int
maxLeavesForNodes nodes = (nodes + 1) `div` 2

genSynTree :: (Int, Int) -> Int -> String -> Int -> Bool -> Maybe (Gen (SynTree Char))
genSynTree (minnode, maxnode) maxdepth lits minuse addoper
    | maxdepth <= 0 || maxnode <= 0 || null lits || length lits < minuse || maxnode < minnode || fst (rangeDepthForNodes minnode) > maxdepth || maxLeavesForNodes maxnode < minuse || 2 ^ (maxdepth - 1) < minuse = Nothing
    | otherwise =  Just $ do
        toUse <- take minuse <$> shuffle lits
        syntaxTree (a,maxnode) maxdepth lits toUse addoper
      where
        a=maximum [0,minnode]

syntaxTree :: Eq c => (Int, Int) -> Int -> [c] -> [c] -> Bool -> Gen (SynTree c)
syntaxTree (minnode, maxnode) maxdepth lits minuse addoper
    | maxdepth == 1 || maxnode == 1 = leafNd
    | minnode == 1 && maxnode == 2 = oneof [ leafNd , negativeLiteral lits minuse]
    | minnode == 2 && maxnode < 3 = negativeLiteral lits minuse
    | minnode <= 2 && maxLeavesForNodes maxnode == length minuse = oneof binaryOper
    | maxLeavesForNodes maxnode == length minuse = oneof binaryOper
    | minnode == 2 && maxnode >= 3 = frequency [(1, negativeForm), (maxnode - minnode - 1, oneof binaryOper)]
    | minnode == 1 && maxnode >= 3 && length minuse <= 1 = frequency [(1,leafNd), (maxnode - minnode - 1, oneof ( negativeForm :  binaryOper))]
    | minnode == 1 && maxnode >= 3 && length minuse > 1= oneof ( negativeForm :  binaryOper)
    | minnode - 1 > maxNodesForDepth (maxdepth - 1) = oneof binaryOper
    | otherwise = oneof ( negativeForm : binaryOper )
      where
        binaryOper = map (binaryOperator (minnode, maxnode) maxdepth lits minuse addoper ) $ chooseList addoper
        negativeForm = negativeFormula (minnode, maxnode) maxdepth lits minuse addoper
        leafNd = leafnode lits $ judgeError minuse

chooseList :: Bool -> [SynTree c -> SynTree c -> SynTree c]
chooseList addoper = if addoper
    then [And, Or, Impl, Equi]
    else [And, Or]

binaryOperator :: Eq c => (Int, Int) -> Int -> [c] -> [c] ->  Bool -> (SynTree c -> SynTree c -> SynTree c) -> Gen (SynTree c)
binaryOperator(minnode, maxnode) maxdepth lits minuse addoper  oper =
    let avoidoveralloc = maximum [0, minnode - 2 - maxNodesForDepth (maxdepth - 1)]
        correctminnode = maximum [3,minnode]
        chooseodd = maxLeavesForNodes maxnode == length minuse
    in do
      radmin <- if chooseodd
        then elements (filter odd [1 + avoidoveralloc..correctminnode - 2 - avoidoveralloc])
        else choose (1 + avoidoveralloc,correctminnode - 2 - avoidoveralloc)
      radmax <- if chooseodd
        then elements (filter odd [radmin..maxnode - correctminnode + radmin])
        else choose (radmin,maxnode - correctminnode + radmin)
      left <- syntaxTree (radmin, radmax) (maxdepth - 1) lits (take (maxLeavesForNodes radmax) minuse) addoper
      right <- syntaxTree (correctminnode - radmin - 1, maxnode - radmax - 1) (maxdepth-1) lits (minuse \\ take (maxLeavesForNodes radmax) minuse) addoper
      return $ oper left right

negativeFormula :: Eq c => (Int, Int) -> Int -> [c] -> [c] -> Bool -> Gen (SynTree c)
negativeFormula (minnode, maxnode) maxdepth lits minus addoper =
    let correctminnode = maximum [2,minnode]
    in do
      e <- syntaxTree (correctminnode - 1, maxnode - 1) (maxdepth - 1) lits minus addoper
      return (Not e)

negativeLiteral :: [c] -> [c] -> Gen (SynTree c)
negativeLiteral lits minus = do
    e <- leafnode lits $ judgeError  minus
    return (Not e)

leafnode :: [c] -> Maybe c -> Gen (SynTree c)
leafnode lits Nothing =do
        e <- elements lits
        return (Leaf e)
leafnode _ (Just minus) = return (Leaf minus)

judgeError :: [c] -> Maybe c
judgeError minuse
    | length minuse > 1 =  error "should not large than 1"
    | otherwise = listToMaybe minuse
