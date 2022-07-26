{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Types
    (
    SynTree(..),

    collectLeaves,
    relabelShape,
    treeNodes,
    treeDepth,
    allNotLeafSubTrees,
    ) where

import Control.Monad.State (get, put, runState)
import Data.Set(fromList, Set)

data SynTree c
    = And {lefttree :: SynTree c, righttree :: SynTree c}
    | Or {lefttree :: SynTree c, righttree :: SynTree c}
    | Impl {lefttree :: SynTree c, righttree :: SynTree c}
    | Equi {lefttree :: SynTree c, righttree :: SynTree c}
    | Not {folltree :: SynTree c}
    | Leaf {leaf :: c}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

collectLeaves :: SynTree c -> [c]
collectLeaves = foldMap ( : [])

relabelShape :: SynTree () -> [c] -> SynTree c
relabelShape shape contents =
    let (tree,[])
          = runState (traverse adorn shape) contents
    in
        tree
    where
      adorn _ =
        do {current <- get; put (tail current); return (head current)}

getNotLeafSubTrees :: SynTree c -> [SynTree c]
getNotLeafSubTrees t@(And a b) = getNotLeafSubTrees a ++ (t : getNotLeafSubTrees b)
getNotLeafSubTrees (Leaf _) =  []
getNotLeafSubTrees t@(Or a b) = getNotLeafSubTrees a ++ (t : getNotLeafSubTrees b)
getNotLeafSubTrees t@(Not a) = t : getNotLeafSubTrees a
getNotLeafSubTrees t@(Impl a b) = getNotLeafSubTrees a ++ (t : getNotLeafSubTrees b)
getNotLeafSubTrees t@(Equi a b) = getNotLeafSubTrees a ++ (t : getNotLeafSubTrees b)

allNotLeafSubTrees :: Ord c => SynTree c -> Set (SynTree c)
allNotLeafSubTrees a = fromList (getNotLeafSubTrees a)


treeNodes :: SynTree c -> Integer
treeNodes (And a b) = 1 + treeNodes a + treeNodes b
treeNodes (Leaf _) =  1
treeNodes (Or a b) = 1 + treeNodes a + treeNodes b
treeNodes (Not a) = 1 + treeNodes a
treeNodes (Impl a b) = 1 + treeNodes a + treeNodes b
treeNodes (Equi a b) = 1 + treeNodes a + treeNodes b

treeDepth :: SynTree c -> Integer
treeDepth (Not a) = 1 + treeDepth a
treeDepth (Leaf _) = 1
treeDepth (And a b) = 1 + max (treeDepth a) (treeDepth b)
treeDepth (Or a b) = 1 + max (treeDepth a) (treeDepth b)
treeDepth (Impl a b) = 1 + max (treeDepth a) (treeDepth b)
treeDepth (Equi a b) = 1 + max (treeDepth a) (treeDepth b)
