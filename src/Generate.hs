module Generate(
 deptharea ,
 genSynTree,
 maxleafnode,
) where

import Types (SynTree(..))
import Test.QuickCheck (choose, elements, oneof, Gen)
import Data.List (isSubsequenceOf, (\\))
import Data.Maybe (isJust, fromJust)

deptharea :: Integer -> (Integer, Integer)
deptharea node = (mindepth, maxdepth)
  where
    mindepth = head [ m | m <-[1,2..], (2 ^ m)-1>=node ]
    maxdepth = node

maxofnode :: Integer -> Integer
maxofnode depth = 2 ^ depth

maxleafnode :: Integer -> Integer
maxleafnode maxnode
    | odd maxnode = maxnode `div` 2+1
    | otherwise = maxnode `div` 2

genSynTree :: (Integer , Integer) -> Integer ->String->String ->Bool-> Maybe (Gen SynTree)
genSynTree (minnode, maxnode) maxdepth lits minuse addoper
    | maxdepth<=0 || maxnode<=0||null lits || maxnode< minnode || fst ( deptharea minnode) > maxdepth||not (isSubsequenceOf minuse lits) || maxleafnode maxnode < toInteger (length minuse)= Nothing
    | otherwise =  Just $ syntaxTree (a,maxnode) maxdepth lits minuse addoper
      where
        a=maximum [0,minnode]

syntaxTree :: (Integer , Integer) -> Integer -> String -> String -> Bool -> Gen SynTree
syntaxTree (minnode, maxnode) maxdepth lits minuse addoper
    | maxdepth == 1 || maxnode == 1 = leafnode lits $ judgminuse minuse
    | minnode == 1 && maxnode == 2 = oneof [ leafnode lits $ judgminuse minuse , negativeLiteral lits minuse]
    | minnode == 2 && maxnode < 3 = negativeLiteral lits minuse
    | minnode <= 2 && maxleafnode maxnode == toInteger (length minuse) = oneof $ binaryOper 3 addoper True
    | maxleafnode maxnode == toInteger (length minuse) = oneof $ binaryOper minnode addoper True
    | minnode == 2 && maxnode >= 3 = oneof [negativeFormula (2, maxnode) maxdepth lits minuse addoper, oneof $ binaryOper 3 addoper False]
    | minnode == 1 && maxnode >= 3 && length minuse <= 1 = oneof [leafnode lits $ judgminuse minuse , oneof ( negativeFormula (2, maxnode) maxdepth lits minuse addoper :  binaryOper 3 addoper False)]
    | minnode == 1 && maxnode >= 3 && length minuse > 1= oneof ( negativeFormula (2, maxnode) maxdepth lits minuse addoper :  binaryOper 3 addoper False)
    | (minnode-1) >= maxofnode ( maxdepth-1) = oneof $ binaryOper minnode addoper False
    | otherwise = oneof ( negativeFormula (minnode, maxnode) maxdepth lits minuse addoper: binaryOper minnode addoper False)
      where
        binaryOper m adder choosodd = map (binaryOperator (m, maxnode) maxdepth lits minuse addoper choosodd) $ chooseList adder

chooseList :: Bool ->[SynTree -> SynTree -> SynTree]
chooseList addoper = if addoper
    then [And, Or, Impl, Equi]
    else [And, Or]

binaryOperator::(Integer , Integer) -> Integer -> String -> String -> Bool -> Bool -> (SynTree -> SynTree -> SynTree) -> Gen SynTree
binaryOperator(minnode, maxnode) maxdepth lits minuse addoper choosodd oper = let a = maximum[0, minnode-1-maxofnode( maxdepth - 1 )]  in do
    radmin <- if choosodd
        then elements (filter odd [1+a..minnode-2-a])
        else choose (1 + a,minnode - 2 - a)
    radmax <- if choosodd
        then elements (filter odd [radmin..maxnode-minnode+radmin])
        else choose (radmin,maxnode - minnode + radmin)
    left <- syntaxTree (radmin, radmax) (maxdepth - 1) lits (take (fromInteger $ maxleafnode radmax) minuse) addoper
    right <- syntaxTree (minnode - radmin - 1, maxnode - radmax - 1) (maxdepth-1) lits (minuse \\  (take $ fromInteger(maxleafnode radmax)) minuse) addoper
    return $ oper left right

negativeFormula::(Integer , Integer) -> Integer -> String -> String -> Bool -> Gen SynTree
negativeFormula (minnode, maxnode) maxdepth lits minus addoper = do
    e <- syntaxTree (minnode - 1, maxnode - 1) (maxdepth - 1) lits minus addoper
    return (Not e)

negativeLiteral :: String -> String -> Gen SynTree
negativeLiteral lits minus = do
    e <- leafnode lits $ judgminuse minus
    return (Not e)

leafnode::String -> Maybe Char -> Gen SynTree
leafnode lits minus =
    if isJust minus then return (Leaf (fromJust minus))
    else do
        e <- elements lits
        return (Leaf e)

judgminuse :: String -> Maybe Char
judgminuse char
    |length char == 1 = Just $ head char
    |otherwise = Nothing