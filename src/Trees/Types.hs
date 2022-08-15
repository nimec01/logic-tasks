{-# LANGUAGE DeriveTraversable #-}

module Trees.Types
    (
    SynTree(..),
    BinOp(..),
    showOperator,
    showOperatorNot,
    allBinaryOperators,
    ) where

data BinOp = And | Or | Impl | Equi
  deriving (Eq, Ord, Show, Enum, Bounded)

showOperator :: BinOp -> String
showOperator And = "/\\"
showOperator Or = "\\/"
showOperator Impl = "=>"
showOperator Equi = "<=>"

allBinaryOperators :: [BinOp]
allBinaryOperators = [minBound .. maxBound]

showOperatorNot :: String
showOperatorNot = "~"

data SynTree o c
    = Binary {op :: o, lefttree :: SynTree o c, righttree :: SynTree o c}
    | Not {folltree :: SynTree o c}
    | Leaf {leaf :: c}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
