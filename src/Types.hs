{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Types
 (
 SynTree(..),
 collectLeaves,
 relabelShape,
 allSubtre,
 )
where

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
collectLeaves = foldMap (:[])

relabelShape :: SynTree () -> [c] -> SynTree c
relabelShape shape contents =
  let (tree,[])
        = runState (traverse adorn shape) contents
  in
    tree
  where
    adorn _ =
      do {current <- get; put (tail current); return (head current)}

gitSubTree :: SynTree c -> [SynTree c]
gitSubTree (And a b) = gitSubTree a ++ (And a b:gitSubTree b)
gitSubTree (Leaf a)=  [Leaf a]
gitSubTree (Or a b) = gitSubTree a ++ (Or a b:gitSubTree b)
gitSubTree (Not a) = Not a:gitSubTree a
gitSubTree (Impl a b) =gitSubTree a ++ (Impl a b:gitSubTree b)
gitSubTree (Equi a b) = gitSubTree a ++ (Equi a b:gitSubTree b)

allSubtre:: Ord c => SynTree c -> Set (SynTree c)
allSubtre a = fromList (sort $ gitSubTree a)