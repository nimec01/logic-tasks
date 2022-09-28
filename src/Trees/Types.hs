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
    = Binary o (SynTree o c) (SynTree o c)
    | Not (SynTree o c)
    | Leaf c
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative (SynTree o) where
  pure = Leaf
  m1 <*> m2 = m1 >>= \f -> m2 >>= \x -> return (f x)

instance Monad (SynTree o) where
  Binary oper a b >>= k = Binary oper (a >>= k) (b >>= k)
  Not a           >>= k = Not (a >>= k)
  Leaf a          >>= k = k a