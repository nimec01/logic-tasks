module Types
  (
    SynTree(..),
    Literal(..),
    depwinode ,
    genSynTree,
    generSynTree
  )
where
import Test.QuickCheck
data SynTree
    = And {lefttree :: SynTree , righttree :: SynTree}
    | Or {lefttree :: SynTree , righttree :: SynTree}
    | Impl {lefttree :: SynTree , righttree :: SynTree}
    | Equi {lefttree :: SynTree , righttree :: SynTree}
    | Not {folltree :: SynTree}
    | Leaf { leaf :: Literal}
    deriving (Eq)

--刚开始就进行syntree的解析直到
-- 先检查括号是否合法
newtype Literal
    = Literal { letter :: Char}
    deriving (Eq )

depwinode :: Integer -> (Integer ,Integer)
depwinode node =(mindepth ,maxdepth)
  where mindepth=head[m |m <-[1,2..], (2 ^ m)-1>=node ]
        maxdepth=node

nodewidep :: Integer ->Integer
nodewidep depth= (2 ^ depth)-1


genSynTree :: (Integer , Integer) -> Integer ->String->Gen SynTree
genSynTree (minnode, maxnode) maxdepth lits --choose 一个以下三个函数
    | maxdepth<0 || maxnode<0||null lits || maxnode< minnode || fst ( depwinode minnode) > maxdepth= error "Can not construct Syntax tree from illegal input."
    | otherwise = generSynTree (a,maxnode) maxdepth lits where
      a=maximum [0,minnode]
--若要添加最小深度可以把最少分配量提前算出来

generSynTree::(Integer , Integer) -> Integer ->String->Gen SynTree
generSynTree (minnode, maxnode) maxdepth lits 
  | minnode == 0 && maxnode > 2= do
    e <-elements [True,False] 
    if e then  genSynTreenosub lits  else oneof [genSynTreewithonesub (minnode, maxnode) maxdepth lits,genSynTreeAnd (minnode, maxnode) maxdepth lits,genSynTreeOr (minnode, maxnode) maxdepth lits,genSynTreeImpl (minnode, maxnode) maxdepth lits,genSynTreeEqui (minnode, maxnode) maxdepth lits]
  | maxnode == 1=genSynTreenosub lits 
  | maxnode == 2 = oneof [genSynTreewithonesub (minnode, maxnode) maxdepth lits,genSynTreenosub lits]
  | (minnode-1)> nodewidep ( maxdepth-1) =oneof [genSynTreeAnd (minnode, maxnode) maxdepth lits,genSynTreeOr (minnode, maxnode) maxdepth lits,genSynTreeImpl (minnode, maxnode) maxdepth lits,genSynTreeEqui (minnode, maxnode) maxdepth lits ]
  | otherwise = oneof [genSynTreewithonesub (minnode, maxnode) maxdepth lits ,genSynTreeAnd (minnode, maxnode) maxdepth lits,genSynTreeOr (minnode, maxnode) maxdepth lits,genSynTreeImpl (minnode, maxnode) maxdepth lits,genSynTreeEqui (minnode, maxnode) maxdepth lits]

genSynTreewithonesub::(Integer , Integer) -> Integer->String->Gen SynTree
genSynTreewithonesub (minnode, maxnode) maxdepth lits = do
  e<-generSynTree (minnode-1,maxnode-1) maxdepth lits 
  return (Not e)
genSynTreeAnd::(Integer , Integer) -> Integer ->String->Gen SynTree
genSynTreeAnd (minnode, maxnode) maxdepth lits =let a=maximum[0,minnode-1-nodewidep(maxdepth-1)] in do   --choose一个Impl之类的
  radmin <-choose (1+a,minnode-1-a)
  radmax <- choose (radmin,maxnode-minnode+radmin)
  left <- generSynTree (radmin,radmax) (maxdepth-1) lits
  right <- generSynTree (minnode-radmin-1,maxnode-radmax-1) (maxnode-maxdepth-1) lits
  return $ And left right
genSynTreeOr::(Integer , Integer) -> Integer ->String->Gen SynTree
genSynTreeOr (minnode, maxnode) maxdepth lits =let a=maximum[0,minnode-1-nodewidep(maxdepth-1)] in do   --choose一个Impl之类的
  radmin <-choose (1+a,minnode-1-a)
  radmax <- choose (radmin,maxnode-minnode+radmin)
  left <- generSynTree (radmin,radmax) (maxdepth-1) lits
  right <- generSynTree (minnode-radmin-1,maxnode-radmax-1) (maxnode-maxdepth-1) lits
  return $ Or left right
genSynTreeImpl::(Integer , Integer) -> Integer ->String->Gen SynTree
genSynTreeImpl (minnode, maxnode) maxdepth lits =let a=maximum[0,minnode-1-nodewidep(maxdepth-1)] in do   --choose一个Impl之类的
  radmin <-choose (1+a,minnode-1-a)
  radmax <- choose (radmin,maxnode-minnode+radmin)
  left <- generSynTree (radmin,radmax) (maxdepth-1) lits
  right <- generSynTree (minnode-radmin-1,maxnode-radmax-1) (maxnode-maxdepth-1) lits
  return $ Impl left right
genSynTreeEqui::(Integer , Integer) -> Integer ->String->Gen SynTree
genSynTreeEqui (minnode, maxnode) maxdepth lits =let a=maximum[0,minnode-1-nodewidep(maxdepth-1)] in do   --choose一个Impl之类的
  radmin <-choose (1+a,minnode-1-a)
  radmax <- choose (radmin,maxnode-minnode+radmin)
  left <- generSynTree (radmin,radmax) (maxdepth-1) lits
  right <- generSynTree (minnode-radmin-1,maxnode-radmax-1) (maxnode-maxdepth-1) lits
  return $ Equi left right

genSynTreenosub::[Char] ->Gen SynTree
genSynTreenosub lits = do
  e<- elements lits
  return (Leaf $ Literal e) 


instance Show SynTree where
  show (And a b) = "(" ++ show a ++"/\\\\"++ show b++")"
  show (Leaf a)= show a 
  show (Or a b) = "(" ++ show a ++"\\\\/"++ show b++")"
  show (Not a) = "~(" ++ show a ++")" 
  show (Impl a b) = "(" ++ show a ++"=>"++ show b ++")"
  show (Equi a b) = "(" ++ show a ++"<=>"++ show b ++")"

instance Show  Literal where
  show a = letter a :""
