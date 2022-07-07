module Types
 (
 SynTree(..),
 collectLeaves,
 allsubtre
 )
where

import Data.List (sort)

data SynTree c
  = And {lefttree :: SynTree c, righttree :: SynTree c}
  | Or {lefttree :: SynTree c, righttree :: SynTree c}
  | Impl {lefttree :: SynTree c, righttree :: SynTree c}
  | Equi {lefttree :: SynTree c, righttree :: SynTree c}
  | Not {folltree :: SynTree c}
  | Leaf {leaf :: c}
  deriving (Eq, Ord, Show)

collectLeaves :: SynTree c -> [c]
collectLeaves (Not a) = collectLeaves a
collectLeaves (Leaf a)= [a]
collectLeaves (And a b) = collectLeaves a ++ collectLeaves b
collectLeaves (Or a b) = collectLeaves a ++ collectLeaves b
collectLeaves (Impl a b) = collectLeaves a ++ collectLeaves b
collectLeaves (Equi a b) = collectLeaves a ++ collectLeaves b

gitSubTree :: SynTree c -> [SynTree c]
gitSubTree (And a b) = gitSubTree a ++ (And a b:gitSubTree b)
gitSubTree (Leaf a)=  [Leaf a]
gitSubTree (Or a b) = gitSubTree a ++ (Or a b:gitSubTree b)
gitSubTree (Not a) = Not a:gitSubTree a
gitSubTree (Impl a b) =gitSubTree a ++ (Impl a b:gitSubTree b)
gitSubTree (Equi a b) = gitSubTree a ++ (Equi a b:gitSubTree b)

allsubtre:: Ord c => SynTree c -> [SynTree c]
allsubtre a = sort $ gitSubTree a
