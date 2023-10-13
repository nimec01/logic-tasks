{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Trees.Types
    (
    SynTree(..),
    BinOp(..),
    FormulaAnswer(..),
    PropFormula(..),
    TreeFormulaAnswer(..),
    showOperator,
    showOperatorNot,
    allBinaryOperators,
    ) where


import GHC.Generics



data BinOp = And | Or | Impl | Equi
  deriving (Eq, Generic, Ord, Show, Enum, Bounded)

showOperator :: BinOp -> String
showOperator And = "∧"
showOperator Or = "∨"
showOperator Impl = "=>"
showOperator Equi = "<=>"

allBinaryOperators :: [BinOp]
allBinaryOperators = [minBound .. maxBound]

showOperatorNot :: String
showOperatorNot = "¬"

data SynTree o c
    = Binary o (SynTree o c) (SynTree o c)
    | Not (SynTree o c)
    | Leaf c
  deriving (Eq, Generic, Ord, Show, Functor, Foldable, Traversable)

instance Applicative (SynTree o) where
  pure = Leaf
  m1 <*> m2 = m1 >>= \f -> m2 >>= \x -> return (f x)

instance Monad (SynTree o) where
  Binary operator a b >>= k = Binary operator (a >>= k) (b >>= k)
  Not a               >>= k = Not (a >>= k)
  Leaf a              >>= k = k a


newtype TreeFormulaAnswer = TreeFormulaAnswer {maybeTree :: Maybe (SynTree BinOp Char)} deriving (Eq, Generic)



instance Show TreeFormulaAnswer where
  show (TreeFormulaAnswer (Just p)) = show p
  show _ = ""


data PropFormula c
    = Atomic c
    | Neg (PropFormula c)
    | Brackets (PropFormula c)
    | Assoc BinOp (PropFormula c) (PropFormula c)
  deriving (Eq, Foldable)




instance Show (PropFormula Char) where
  show (Atomic c) = [c]
  show (Neg f) = showOperatorNot ++ show f
  show (Brackets f) = '(' : show f ++ ")"
  show (Assoc o f1 f2) = show f1 ++ " " ++ showOperator o ++ " " ++ show f2



newtype FormulaAnswer = FormulaAnswer {maybeForm :: Maybe (PropFormula Char)} deriving (Eq, Generic)

instance Show FormulaAnswer where
  show (FormulaAnswer (Just p)) = show p
  show _ = ""
