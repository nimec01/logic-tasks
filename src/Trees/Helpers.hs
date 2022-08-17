{-# LANGUAGE FlexibleContexts #-}

module Trees.Helpers
    (
    numberAllBinaryNodes,
    collectLeaves,
    relabelShape,
    treeNodes,
    treeDepth,
    allNotLeafSubTrees,
    minDepthForNodes,
    maxLeavesForNodes,
    maxNodesForDepth,
    noSameSubTree,
    sameAssociativeOperatorAdjacent,
    similarExist,
    consecutiveNegations,
    ) where

import Control.Monad.State (get, put, runState, evalState)
import Data.Set(fromList, Set)
import Data.List.Extra (nubOrd, nubBy)

import Trees.Types (SynTree(..), BinOp(..))

numberAllBinaryNodes :: SynTree o c -> SynTree (o, Integer) c
numberAllBinaryNodes = flip evalState 1 . go
    where
      go (Leaf c) = return (Leaf c)
      go (Not t) = Not <$> go t
      go (Binary o t1 t2) = do { l <- next; t1' <- go t1; t2' <- go t2; return (Binary (o,l) t1' t2') }
      next = do {current <- get; put (current + 1); return current}

collectLeaves :: SynTree o c -> [c]
collectLeaves = foldMap ( : [])

relabelShape :: SynTree o () -> [c] -> SynTree o c
relabelShape shape contents =
    let (tree,[])
          = runState (traverse adorn shape) contents
    in
        tree
    where
      adorn _ =
        do {current <- get; put (tail current); return (head current)}

getNotLeafSubTrees :: SynTree o c -> [SynTree o c]
getNotLeafSubTrees t@(Binary _ a b) = getNotLeafSubTrees a ++ (t : getNotLeafSubTrees b)
getNotLeafSubTrees (Leaf _) =  []
getNotLeafSubTrees t@(Not a) = t : getNotLeafSubTrees a

allNotLeafSubTrees :: (Ord o, Ord c) => SynTree o c -> Set (SynTree o c)
allNotLeafSubTrees a = fromList (getNotLeafSubTrees a)

noSameSubTree :: (Eq o, Ord o, Ord c) => SynTree o c -> Bool
noSameSubTree synTree = let treeList = getNotLeafSubTrees synTree
    in
        treeList == nubOrd treeList

treeNodes :: SynTree o c -> Integer
treeNodes (Binary _ a b) = 1 + treeNodes a + treeNodes b
treeNodes (Leaf _) =  1
treeNodes (Not a) = 1 + treeNodes a

treeDepth :: SynTree o c -> Integer
treeDepth (Not a) = 1 + treeDepth a
treeDepth (Leaf _) = 1
treeDepth (Binary _ a b) = 1 + max (treeDepth a) (treeDepth b)

minDepthForNodes :: Integer -> Integer
minDepthForNodes nodes = ceiling (logBase 2 (fromIntegral (nodes + 1) :: Float))

maxNodesForDepth :: Integer -> Integer
maxNodesForDepth depth = 2 ^ depth - 1

maxLeavesForNodes :: Integer -> Integer
maxLeavesForNodes nodes = (nodes + 1) `div` 2

sameAssociativeOperatorAdjacent :: SynTree BinOp c -> Bool
sameAssociativeOperatorAdjacent (Leaf _) = False
sameAssociativeOperatorAdjacent (Not a) = sameAssociativeOperatorAdjacent a
sameAssociativeOperatorAdjacent (Binary oper a b) = checkNextOperator a oper || checkNextOperator b oper || sameAssociativeOperatorAdjacent a || sameAssociativeOperatorAdjacent b

checkNextOperator :: SynTree BinOp c -> BinOp -> Bool
checkNextOperator (Binary And _ _) And = True
checkNextOperator (Binary Or _ _) Or = True
checkNextOperator _ _ = False

similarTree :: Eq o => SynTree o c -> SynTree o c -> Bool
similarTree (Binary o1 a b) (Binary o2 c d) | o1 == o2 = similarTree a c && similarTree b d
similarTree (Not a) (Not c) = similarTree a c
similarTree (Leaf _) (Leaf _) = True
similarTree _ _ = False

similarExist :: Eq o => [SynTree o c] -> Bool
similarExist trees = length (nubBy similarTree trees) /= length trees

consecutiveNegations :: SynTree o c -> Integer
consecutiveNegations (Binary _ a b) = max (consecutiveNegations a) (consecutiveNegations b)
consecutiveNegations (Not a) = max (consecutiveNegations a) (1 + continueNot a)
consecutiveNegations (Leaf _)  = 0

continueNot :: SynTree o c -> Integer
continueNot (Not a) = 1 + continueNot a
continueNot _ = 0
