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
getSubTrees t@(And a b) = getSubTrees a ++ (t : getSubTrees b)
getSubTrees t@(Leaf _) =  [t]
getSubTrees t@(Or a b) = getSubTrees a ++ (t : getSubTrees b)
getSubTrees t@(Not a) = t : getSubTrees a
getSubTrees t@(Impl a b) = getSubTrees a ++ (t : getSubTrees b)
getSubTrees t@(Equi a b) = getSubTrees a ++ (t : getSubTrees b)

allSubtrees :: Ord c => SynTree c -> Set (SynTree c)
allSubtrees a = fromList (getSubTrees a)

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
