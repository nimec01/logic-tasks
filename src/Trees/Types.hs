{-# LANGUAGE DeriveTraversable #-}

module Trees.Types
    (
    SynTree(..),
    Op(..),
    showOperator,
    ) where

data Op = And | Or | Impl | Equi | Not
  deriving (Eq, Ord, Show)

showOperator :: Op -> String
showOperator And = "/\\"
showOperator Or = "\\/"
showOperator Impl = "=>"
showOperator Equi = "<=>"
showOperator Not = "~"

data SynTree o c
    = Binary {op :: o, lefttree :: SynTree o c, righttree :: SynTree o c}
    | Unary {op :: o, folltree :: SynTree o c}
    | Leaf {leaf :: c}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
