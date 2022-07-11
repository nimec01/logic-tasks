{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Types
    (
    SynTree(..),

    collectLeaves,
    relabelShape,
    allSubtree,
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

getSubTree :: SynTree c -> [SynTree c]
getSubTree (And a b) = getSubTree a ++ (And a b : getSubTree b)
getSubTree (Leaf a) =  [Leaf a]
getSubTree (Or a b) = getSubTree a ++ (Or a b : getSubTree b)
getSubTree (Not a) = Not a : getSubTree a
getSubTree (Impl a b) = getSubTree a ++ (Impl a b : getSubTree b)
getSubTree (Equi a b) = getSubTree a ++ (Equi a b : getSubTree b)

allSubtree :: Ord c => SynTree c -> Set (SynTree c)
allSubtree a = fromList (sort $ getSubTree a)
