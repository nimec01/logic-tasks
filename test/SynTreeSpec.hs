module SynTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck ( chooseInt, sublistOf, forAll, Gen )
import Types ( depwinode, genSynTree, SynTree (And ,Not, Or,Impl,Equi,Leaf),maxleafnode )
import Parsing ( normParse )
import Data.Char (isLetter)
import qualified Control.Exception as Exc (evaluate)
import Data.Maybe ( fromJust, isNothing,fromMaybe )
import Data.List (intersect)

-- chooseletter :: Bool -> Char ->Bool
-- chooseletter False _ = False
-- chooseletter _ t =isLetter t
nodenum :: SynTree -> Integer
nodenum (Not a) = 1+nodenum a
nodenum (Leaf a)= 1
nodenum (And a b) = 1+nodenum a+nodenum b
nodenum (Or a b) = 1+nodenum a+nodenum b
nodenum (Impl a b) = 1+nodenum a+nodenum b
nodenum (Equi a b) = 1+nodenum a+nodenum b

treedepth:: SynTree-> Integer
treedepth (Not a) = 1 + treedepth a
treedepth (Leaf a)= 1
treedepth (And a b) = 1 + maximum [treedepth a,treedepth b]
treedepth (Or a b) = 1 + maximum [treedepth a,treedepth b]
treedepth (Impl a b) = 1 + maximum [treedepth a,treedepth b]
treedepth (Equi a b) = 1 + maximum [treedepth a,treedepth b]


--takeWhile :: (a -> Bool) -> [a] -> [a]  intersect :: Eq a => [a] -> [a] -> [a]
usestring :: String ->SynTree->Bool
usestring str synTree = str ==(intersect str (catchstr synTree))

catchstr :: SynTree-> String
catchstr (Not a) = catchstr a
catchstr (Leaf a)= a:""
catchstr (And a b) = catchstr a ++ catchstr b
catchstr (Or a b) =  catchstr a ++ catchstr b
catchstr (Impl a b) =  catchstr a ++ catchstr b
catchstr (Equi a b) =  catchstr a ++ catchstr b


invalidBoundsSyntr :: Gen ((Integer,Integer),Integer,String,String)
invalidBoundsSyntr = do
    validChars <- sublistOf ['A'..'Z']
    minnode <- chooseInt (2,100)
    maxnode <- chooseInt (1,minnode-1)
    maxdepth <- chooseInt (fromInteger $ fst(depwinode $toInteger  minnode),maxnode)
    pure ((toInteger minnode,toInteger maxnode), toInteger maxdepth ,validChars,validChars)

validBoundsSyntr :: Gen ((Integer,Integer),Integer,String,String)
validBoundsSyntr = do
    validChars <- sublistOf ['A'..'Z']
    minnode <- chooseInt (1,100)
    maxnode <- chooseInt (minnode,100)
    maxdepth <- chooseInt (fromInteger $ fst(depwinode $toInteger  minnode),maxnode)
    pure ((toInteger minnode,toInteger maxnode), toInteger maxdepth ,validChars,(take $ fromInteger(maxleafnode $ toInteger maxnode)) validChars)


validBoundsSyntr2 :: Gen ((Integer,Integer),Integer,String,String)
validBoundsSyntr2 = do
    validChars <- sublistOf ['A'..'Z']
    minnode <- chooseInt (1,100)
    maxnode <- chooseInt (minnode,100)
    pure ((toInteger minnode,toInteger maxnode), toInteger maxnode ,validChars,(take $ fromInteger(maxleafnode $ toInteger maxnode)) validChars)

-- genNothing ::Gen SynTree
-- genNothing =return $  Leaf '1'

-- genSynTree :: (Integer , Integer) -> Integer ->String-> Maybe (Gen SynTree)
spec :: Spec
spec = describe "genSyntaxTree" $ do
        it "should generate a random SyntaxTree from the given parament and can be parsed by normParse" $
            forAll validBoundsSyntr $ \((minnode,maxnode), maxdepth ,validChars,minuse)->forAll (fromJust $ genSynTree (minnode,maxnode) maxdepth validChars minuse)  $ \synTree -> normParse (show synTree)==Right synTree
        -- it "should throw an error call" $
        --     forAll invalidBoundsSyntr $ \((minnode,maxnode), maxdepth ,validChars,minuse)->forAll (fromMaybe genNothing $ genSynTree (minnode,maxnode) maxdepth validChars minuse)  $ \synTree -> synTree== Leaf '1'
        it "should generate a random SyntaxTree from the given parament and in the node area" $
            forAll validBoundsSyntr $ \((minnode,maxnode), maxdepth ,validChars,minuse)->forAll (fromJust $ genSynTree (minnode,maxnode) maxdepth validChars minuse)  $ \synTree -> nodenum synTree>=minnode &&nodenum synTree<=maxnode
        it "should generate a random SyntaxTree from the given parament and not deeper than the maxdepth" $
            forAll validBoundsSyntr $ \((minnode,maxnode), maxdepth ,validChars,minuse)->forAll (fromJust $ genSynTree (minnode,maxnode) maxdepth validChars  minuse)  $ \synTree -> treedepth synTree<= maxdepth
        it "should generate a random SyntaxTree from the given parament and use all of the string it must use" $
            forAll validBoundsSyntr2 $ \((minnode,maxnode), maxdepth ,validChars,minuse)->forAll (fromJust $ genSynTree (minnode,maxnode) maxdepth validChars  minuse)  $ usestring minuse