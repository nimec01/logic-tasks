module Types
  (
    SynTree(..),
    Literal(..)
  )
where

data SynTree
    = And {lefttree :: SynTree , righttree :: SynTree}
    | Or {lefttree :: SynTree , righttree :: SynTree}
    | Impl {lefttree :: SynTree , righttree :: SynTree}
    | Equi {lefttree :: SynTree , righttree :: SynTree}
    | Not {folltree :: SynTree}
    | Leaf { leaf :: Literal}
    deriving (Eq,Show)

--刚开始就进行syntree的解析直到
-- 先检查括号是否合法
newtype Literal
    = Literal { letter :: Char}
    deriving (Eq,Show)


                                          