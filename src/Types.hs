{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Types
    (
    SynTree(..),

    collectLeaves,
    relabelShape,
    allSubtrees,
    treeNodes,
    treeDepth,
    ) where

import Data.List (sort)
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

getSubTrees :: SynTree c -> [SynTree c]
getSubTrees (And a b) = getSubTrees a ++ (And a b : getSubTrees b)
getSubTrees (Leaf a) =  [Leaf a]
getSubTrees (Or a b) = getSubTrees a ++ (Or a b : getSubTrees b)
getSubTrees (Not a) = Not a : getSubTrees a
getSubTrees (Impl a b) = getSubTrees a ++ (Impl a b : getSubTrees b)
getSubTrees (Equi a b) = getSubTrees a ++ (Equi a b : getSubTrees b)

allSubtrees :: Ord c => SynTree c -> Set (SynTree c)
allSubtrees a = fromList (sort $ getSubTrees a)

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
