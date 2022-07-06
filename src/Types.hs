module Types
 (
 SynTree(..),
 allsubtre
 )
where

import Data.List (sort)
data SynTree
 = And {lefttree :: SynTree , righttree :: SynTree}
  | Or {lefttree :: SynTree , righttree :: SynTree}
  | Impl {lefttree :: SynTree , righttree :: SynTree}
  | Equi {lefttree :: SynTree , righttree :: SynTree}
  | Not {folltree :: SynTree}
  | Leaf { leaf :: Char}
  deriving (Eq, Ord,Show )

gitSubTree :: SynTree -> [SynTree]
gitSubTree (And a b) = gitSubTree a ++ (And a b:gitSubTree b)
gitSubTree (Leaf a)=  [Leaf a]
gitSubTree (Or a b) = gitSubTree a ++ (Or a b:gitSubTree b)
gitSubTree (Not a) = Not a:gitSubTree a
gitSubTree (Impl a b) =gitSubTree a ++ (Impl a b:gitSubTree b)
gitSubTree (Equi a b) = gitSubTree a ++ (Equi a b:gitSubTree b)

allsubtre:: SynTree -> [SynTree]
allsubtre a = sort $ gitSubTree a