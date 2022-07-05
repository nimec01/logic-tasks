module Generate(
 deptharea ,
 genSynTree,
 maxleafnode,
) where

import Types(SynTree(..))
import Test.QuickCheck
 ( choose, elements, oneof, Gen )
import Data.List (isSubsequenceOf, (\\))
deptharea :: Integer -> (Integer ,Integer)
deptharea node =(mindepth ,maxdepth)
  where mindepth=head[m |m <-[1,2..], (2 ^ m)-1>=node ]
        maxdepth=node

maxofnode :: Integer ->Integer
maxofnode depth= 2 ^ depth

maxleafnode :: Integer -> Integer
maxleafnode maxnode
 | odd maxnode =maxnode `div` 2+1
 | otherwise =maxnode `div` 2

genSynTree :: (Integer , Integer) -> Integer ->String->String-> Maybe (Gen SynTree)
genSynTree (minnode, maxnode) maxdepth lits minuse
 | maxdepth<=0 || maxnode<=0||null lits || maxnode< minnode || fst ( deptharea minnode) > maxdepth||(not $ isSubsequenceOf minuse lits) ||(maxleafnode maxnode)< (toInteger $ length minuse)= Nothing
 | otherwise =  Just $ generSynTree (a,maxnode) maxdepth lits minuse
  where
  a=maximum [0,minnode]

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
 | (minnode-1)>= maxofnode ( maxdepth-1) =oneof $ gentwoSub minnode
 | otherwise = oneof ( genSynTreewithonesub (minnode, maxnode) maxdepth lits minuse : gentwoSub minnode)
 where
  gentwoSub m = map (genSynTreewithtwosub (m, maxnode) maxdepth lits minuse) [And, Or, Impl, Equi]
  gentwoSub2 m = map (genSynTreewithtwosub2 (m, maxnode) maxdepth lits minuse) [And, Or, Impl, Equi]

genSynTreewithtwosub::(Integer , Integer) -> Integer ->String->String->(SynTree -> SynTree -> SynTree)->Gen SynTree
genSynTreewithtwosub(minnode, maxnode) maxdepth lits minuse oper  = let a=maximum[0,minnode-1-maxofnode(maxdepth-1)]  in do   --choose一个Impl之类的
 radmin <-choose (1+a,minnode-2-a)
 radmax <- choose (radmin,maxnode-minnode+radmin)
 left <- generSynTree (radmin,radmax) (maxdepth-1) lits (take (fromInteger $ maxleafnode radmax) minuse)
 right <- generSynTree (minnode-radmin-1,maxnode-radmax-1) (maxdepth-1) lits (minuse \\  ((take $ fromInteger(maxleafnode radmax)) minuse))
 return $ oper left right

genSynTreewithtwosub2::(Integer , Integer) -> Integer ->String->String->(SynTree -> SynTree -> SynTree)->Gen SynTree
genSynTreewithtwosub2(minnode, maxnode) maxdepth lits minuse oper  = let a=maximum[0,minnode-1-maxofnode(maxdepth-1)]  in do   --choose一个Impl之类的
 radmin <-elements (filter odd [1+a..minnode-2-a])
 radmax <- elements (filter odd [radmin..maxnode-minnode+radmin])
 left <- generSynTree (radmin,radmax) (maxdepth-1) lits (take (fromInteger $ maxleafnode radmax) minuse)
 right <- generSynTree (minnode-radmin-1,maxnode-radmax-1) (maxdepth-1) lits (minuse \\  ((take $ fromInteger(maxleafnode radmax)) minuse))
 return $ oper left right

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