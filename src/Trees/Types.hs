{-# LANGUAGE DeriveTraversable #-}

module Trees.Types
    (
    SynTree(..),
    Op(..),
    showOperator,
    allOperators,
    allBinaryOperators,
    ) where

import Data.List ((\\))

data Op = And | Or | Impl | Equi | Not
  deriving (Eq, Ord, Show, Enum, Bounded)

showOperator :: Op -> String
showOperator And = "/\\"
showOperator Or = "\\/"
showOperator Impl = "=>"
showOperator Equi = "<=>"
showOperator Not = "~"

allOperators :: [Op]
allOperators = [minBound .. maxBound]

allBinaryOperators :: [Op]
allBinaryOperators = allOperators \\ [Not]

data SynTree o c
    = Binary {op :: o, lefttree :: SynTree o c, righttree :: SynTree o c}
    | Unary {op :: o, folltree :: SynTree o c}
    | Leaf {leaf :: c}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
