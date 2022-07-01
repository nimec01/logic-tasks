module Types
  (
    SynTree(..),
    depwinode ,
    genSynTree
  )
where

import Test.QuickCheck
    ( choose, elements, oneof, sized, Arbitrary(arbitrary), Gen )

data SynTree
    = And {lefttree :: SynTree , righttree :: SynTree}
    | Or {lefttree :: SynTree , righttree :: SynTree}
    | Impl {lefttree :: SynTree , righttree :: SynTree}
    | Equi {lefttree :: SynTree , righttree :: SynTree}
    | Not {folltree :: SynTree}
    | Leaf { leaf :: Char}
    deriving (Eq)

--刚开始就进行syntree的解析直到
-- 先检查括号是否合法

depwinode :: Integer -> (Integer ,Integer)
depwinode node =(mindepth ,maxdepth)
  where mindepth=head[m |m <-[1,2..], (2 ^ m)-1>=node ]
        maxdepth=node

nodewidep :: Integer ->Integer
nodewidep depth= 2 ^ depth

genSynTree :: (Integer , Integer) -> Integer ->String-> Maybe (Gen SynTree)
genSynTree (minnode, maxnode) maxdepth lits --choose 一个以下三个函数
    | maxdepth<=0 || maxnode<=0||null lits || maxnode< minnode || fst ( depwinode minnode) > maxdepth= Nothing
    | otherwise =  Just $ generSynTree (a,maxnode) maxdepth lits
       where
      a=maximum [0,minnode]
--若要添加最小深度可以把最少分配量提前算出来
--map (genSynTreewithtwosub (minnode, maxnode) maxdepth lits) [And, Or, Impl, Equi]

generSynTree::(Integer , Integer) -> Integer ->String->Gen SynTree
generSynTree (minnode, maxnode) maxdepth lits
  | maxdepth ==1 =genSynTreenosub lits
  | minnode == 1 &&maxnode < 3 = genSynTreenosub lits
  | minnode == 2 &&maxnode < 3 =genSynTreewithonesub (2, maxnode) maxdepth lits
  | minnode == 2 && maxnode >= 3= do
    e <-elements [True,False]
    if e then  genSynTreewithonesub (2, maxnode) maxdepth lits  else oneof $ gentwoSub 3
  | minnode == 1 && maxnode >= 3= do
    e <-elements [True,False]
    if e then  genSynTreenosub lits  else oneof ( genSynTreewithonesub (2, maxnode) maxdepth lits :  gentwoSub 3 )
  | (minnode-1)>= nodewidep ( maxdepth-1) =oneof $ gentwoSub minnode
  | otherwise = oneof ( genSynTreewithonesub (minnode, maxnode) maxdepth lits : gentwoSub minnode)
  where
    gentwoSub m = map (genSynTreewithtwosub (m, maxnode) maxdepth lits) [And, Or, Impl, Equi]

genSynTreewithtwosub::(Integer , Integer) -> Integer ->String->(SynTree -> SynTree -> SynTree)->Gen SynTree
genSynTreewithtwosub(minnode, maxnode) maxdepth lits oper  = let a=maximum[0,minnode-1-nodewidep(maxdepth-1)] in do   --choose一个Impl之类的
  radmin <-choose (1+a,minnode-2-a)
  radmax <- choose (radmin,maxnode-minnode+radmin)
  left <- generSynTree (radmin,radmax) (maxdepth-1) lits
  right <- generSynTree (minnode-radmin-1,maxnode-radmax-1) (maxdepth-1) lits
  return $ oper left right

genSynTreewithonesub::(Integer , Integer) -> Integer->String->Gen SynTree
genSynTreewithonesub (minnode, maxnode) maxdepth lits = do
  e<-generSynTree (minnode-1,maxnode-1) (maxdepth-1) lits
  return (Not e)

genSynTreenosub::String ->Gen SynTree
genSynTreenosub lits = do
  e<- elements lits
  return (Leaf e)

instance Show SynTree where
  show (And a b) = normalshow a ++"/\\"++ normalshow b
  show (Leaf a)=  a:""
  show (Or a b) = normalshow a ++"\\/"++ normalshow b
  show (Not a) = "~" ++ normalshow a ++""
  show (Impl a b) = normalshow a ++"=>"++ normalshow b
  show (Equi a b) = normalshow a ++"<=>"++ normalshow b

normalshow:: SynTree-> String
normalshow (And a b) = "(" ++ normalshow a ++"/\\"++ normalshow b++")"
normalshow (Leaf a)=  a:""
normalshow (Or a b) = "(" ++ normalshow a ++"\\/"++ normalshow b++")"
normalshow (Not a) = "~" ++ normalshow a ++""
normalshow (Impl a b) = "(" ++ normalshow a ++"=>"++ normalshow b ++")"
normalshow (Equi a b) = "(" ++ normalshow a ++"<=>"++ normalshow b ++")"

instance Arbitrary SynTree where
    -- arbitrary= genSynTree (8,10) 5 ['A','B','C']
  arbitrary= sized syntr
    where
      syntr :: Int -> Gen SynTree
      syntr n
       |n==0 = generSynTree (1,1) 1 ['A']
       |otherwise = do
         let
           m=abs n
           lits = take m ['A'..'Z']
           depth=fst (depwinode (toInteger n))+1
         a <- choose (1,m)
         b <- choose (a,m)
         generSynTree (toInteger a,toInteger b) depth lits
    -- a <- choose (1,20)
    -- b <- choose (a,20)
    -- genSynTree (a,b) depth [list]
    -- where
    --   depth=fst (depwinode a)