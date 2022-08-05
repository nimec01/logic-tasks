{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Types
    (
    SynTree(..),
    Op(..),

    numberAllNodes,
    collectLeaves,
    relabelShape,
    treeNodes,
    treeDepth,
    allNotLeafSubTrees,
    getNotLeafSubTrees,
    ) where

import Control.Monad.State (get, put, runState, evalState)
import Data.Set(fromList, Set)

data Op = And | Or | Impl | Equi | Not
  deriving (Eq, Ord, Show)

data SynTree o c
    = Binary {op :: o, lefttree :: SynTree o c, righttree :: SynTree o c}
    | Unary {op :: o, folltree :: SynTree o c}
    | Leaf {leaf :: c}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

numberAllNodes :: SynTree o c -> SynTree (o, Integer) (c, Integer)
numberAllNodes = flip evalState 1 . go
    where
      go (Leaf c) = do { l <- next; return (Leaf (c,l)) }
      go (Unary o t) = do { l <- next; t' <- go t; return (Unary (o,l) t') }
      go (Binary o t1 t2) = do { l <- next; t1' <- go t1; t2' <- go t2; return (Binary (o,l) t1' t2') }
      next = do {current <- get; put (current + 1); return current}

collectLeaves :: SynTree Op c -> [c]
collectLeaves = foldMap ( : [])

relabelShape :: SynTree Op () -> [c] -> SynTree Op c
relabelShape shape contents =
    let (tree,[])
          = runState (traverse adorn shape) contents
    in
        tree
    where
      adorn _ =
        do {current <- get; put (tail current); return (head current)}

getNotLeafSubTrees :: SynTree Op c -> [SynTree Op c]
getNotLeafSubTrees t@(Binary _ a b) = getNotLeafSubTrees a ++ (t : getNotLeafSubTrees b)
getNotLeafSubTrees (Leaf _) =  []
getNotLeafSubTrees t@(Unary _ a) = t : getNotLeafSubTrees a

allNotLeafSubTrees :: Ord c => SynTree Op c -> Set (SynTree Op c)
allNotLeafSubTrees a = fromList (getNotLeafSubTrees a)

treeNodes :: SynTree Op c -> Integer
treeNodes (Binary _ a b) = 1 + treeNodes a + treeNodes b
treeNodes (Leaf _) =  1
treeNodes (Unary _ a) = 1 + treeNodes a

treeDepth :: SynTree Op c -> Integer
treeDepth (Unary _ a) = 1 + treeDepth a
treeDepth (Leaf _) = 1
treeDepth (Binary _ a b) = 1 + max (treeDepth a) (treeDepth b)
