module Types
  (
    SynTree(..),
    depwinode ,
    genSynTree,
    maxleafnode,
    allsubtre
  )
where

import Test.QuickCheck
    ( choose, elements, oneof, Gen )-- , sized, Arbitrary(arbitrary))
import Data.List (isSubsequenceOf, (\\),sort)
data SynTree
    = And {lefttree :: SynTree , righttree :: SynTree}
    | Or {lefttree :: SynTree , righttree :: SynTree}
    | Impl {lefttree :: SynTree , righttree :: SynTree}
    | Equi {lefttree :: SynTree , righttree :: SynTree}
    | Not {folltree :: SynTree}
    | Leaf { leaf :: Char}
    deriving (Eq,Ord )

--刚开始就进行syntree的解析直到
-- 先检查括号是否合法

depwinode :: Integer -> (Integer ,Integer)
depwinode node =(mindepth ,maxdepth)
  where mindepth=head[m |m <-[1,2..], (2 ^ m)-1>=node ]
        maxdepth=node

nodewidep :: Integer ->Integer
nodewidep depth= 2 ^ depth

maxleafnode :: Integer -> Integer
maxleafnode maxnode
  | odd maxnode =maxnode `div` 2+1
  | otherwise =maxnode `div` 2

-- maxadjust ::Integer -> Integer
-- maxadjust node
--   | odd node =node
--   | otherwise =node+1

genSynTree :: (Integer , Integer) -> Integer ->String->String-> Maybe (Gen SynTree)
genSynTree (minnode, maxnode) maxdepth lits minuse --choose 一个以下三个函数
    | maxdepth<=0 || maxnode<=0||null lits || maxnode< minnode || fst ( depwinode minnode) > maxdepth||(not $ isSubsequenceOf minuse lits) ||(maxleafnode maxnode)< (toInteger $ length minuse)= Nothing
    | otherwise =  Just $ generSynTree (a,maxnode) maxdepth lits minuse
       where
      a=maximum [0,minnode]
--若要添加最小深度可以把最少分配量提前算出来
--map (genSynTreewithtwosub (minnode, maxnode) maxdepth lits) [And, Or, Impl, Equi]

generSynTree::(Integer , Integer) -> Integer ->String->String->Gen SynTree
generSynTree (minnode, maxnode) maxdepth lits minuse
  | maxdepth ==1 =genSynTreenosub lits minuse
  | minnode == 1 &&maxnode < 3 = genSynTreenosub lits minuse
  | minnode == 2 &&maxnode < 3 =genSynTreewithonesub (2, maxnode) maxdepth lits minuse
  | minnode <= 2 && (maxleafnode maxnode)== (toInteger $ length minuse) = oneof $ gentwoSub2 3
  | (maxleafnode maxnode)== (toInteger $ length minuse) = oneof $ gentwoSub2 minnode
  | minnode == 2 && maxnode >= 3 = do
    e <-elements [True,False]
    if e then  genSynTreewithonesub (2, maxnode) maxdepth lits minuse  else oneof $ gentwoSub 3
  | minnode == 1 && maxnode >= 3 && length minuse<=1= do
    e <-elements [True,False]
    if e then  genSynTreenosub lits minuse else oneof ( genSynTreewithonesub (2, maxnode) maxdepth lits minuse :  gentwoSub 3 )
  | minnode == 1 && maxnode >= 3 && length minuse > 1= oneof ( genSynTreewithonesub (2, maxnode) maxdepth lits minuse :  gentwoSub 3 )
  | (minnode-1)>= nodewidep ( maxdepth-1) =oneof $ gentwoSub minnode
  | otherwise = oneof ( genSynTreewithonesub (minnode, maxnode) maxdepth lits minuse : gentwoSub minnode)
  where
    gentwoSub m = map (genSynTreewithtwosub (m, maxnode) maxdepth lits minuse) [And, Or, Impl, Equi]
    gentwoSub2 m = map (genSynTreewithtwosub2 (m, maxnode) maxdepth lits minuse) [And, Or, Impl, Equi]

genSynTreewithtwosub::(Integer , Integer) -> Integer ->String->String->(SynTree -> SynTree -> SynTree)->Gen SynTree
genSynTreewithtwosub(minnode, maxnode) maxdepth lits minuse oper  = let a=maximum[0,minnode-1-nodewidep(maxdepth-1)]  in do   --choose一个Impl之类的
  radmin <-choose (1+a,minnode-2-a)
  radmax <- choose (radmin,maxnode-minnode+radmin)
  left <- generSynTree (radmin,radmax) (maxdepth-1) lits (take (fromInteger $ maxleafnode radmax) minuse)
  right <- generSynTree (minnode-radmin-1,maxnode-radmax-1) (maxdepth-1) lits (minuse \\  ((take $ fromInteger(maxleafnode radmax)) minuse))
  return $ oper left right
  -- radlenth <- choose(1,length minuse)
  -- radmax <- choose((2*radlenth - 1),((fromInteger maxnode)- ((length minuse - radlenth)*2-1)-1))
  -- radmin <- choose (maximum [1+(fromInteger a),(fromInteger (minnode-maxnode))+radmax],radmax)
  -- left <- generSynTree (toInteger radmin,toInteger radmax) (maxdepth-1) lits (take  radlenth minuse)
  -- right <- generSynTree (minnode-toInteger radmin-1,maxnode-toInteger radmax-1) (maxdepth-1) lits (minuse \\  ((take radlenth) minuse))
  -- return $ oper left right

genSynTreewithtwosub2::(Integer , Integer) -> Integer ->String->String->(SynTree -> SynTree -> SynTree)->Gen SynTree
genSynTreewithtwosub2(minnode, maxnode) maxdepth lits minuse oper  = let a=maximum[0,minnode-1-nodewidep(maxdepth-1)]  in do   --choose一个Impl之类的
  radmin <-elements (filter odd [1+a..minnode-2-a])
  radmax <- elements (filter odd [radmin..maxnode-minnode+radmin])
  left <- generSynTree (radmin,radmax) (maxdepth-1) lits (take (fromInteger $ maxleafnode radmax) minuse)
  right <- generSynTree (minnode-radmin-1,maxnode-radmax-1) (maxdepth-1) lits (minuse \\  ((take $ fromInteger(maxleafnode radmax)) minuse))
  return $ oper left right



  -- where
  --   minu1=maxleafnode radmax  (take (maxleafnode radmax) minuse)


genSynTreewithonesub::(Integer , Integer) -> Integer->String->String->Gen SynTree
genSynTreewithonesub (minnode, maxnode) maxdepth lits minus= do
  e<-generSynTree (minnode-1,maxnode-1) (maxdepth-1) lits minus
  return (Not e)

genSynTreenosub::String -> String->Gen SynTree
genSynTreenosub lits minus =
  if minus=="" then do
    e <- elements lits
    return (Leaf e)
  else do
    e <- elements minus
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

-- instance Arbitrary SynTree where
--     -- arbitrary= genSynTree (8,10) 5 ['A','B','C']
--   arbitrary= sized syntr
--     where
--       syntr :: Int -> Gen SynTree
--       syntr n
--        |n==0 = generSynTree (1,1) 1 "A" "A"
--        |otherwise = do
--          let
--            m=abs n
--            lits = take m ['A'..'Z']
--            depth=fst (depwinode (toInteger n))+1
--          a <- choose (1,m)
--          b <- choose (a,m)
--          generSynTree (toInteger a,toInteger b) depth lits lits
    -- a <- choose (1,20)
    -- b <- choose (a,20)
    -- genSynTree (a,b) depth [list]
    -- where
    --   depth=fst (depwinode a)
gitSubTree :: SynTree -> [SynTree]
gitSubTree (And a b) = gitSubTree a ++ (And a b:gitSubTree b)
gitSubTree (Leaf a)=  [Leaf a]
gitSubTree (Or a b) = gitSubTree a ++ (Or a b:gitSubTree b)
gitSubTree (Not a) = Not a:gitSubTree a
gitSubTree (Impl a b) =gitSubTree a ++ (Impl a b:gitSubTree b)
gitSubTree (Equi a b) = gitSubTree a ++ (Equi a b:gitSubTree b)

allsubtre:: SynTree -> [SynTree]
allsubtre a = sort $ gitSubTree a