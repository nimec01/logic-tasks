module SynTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck ( chooseInt, sublistOf, forAll, Gen,elements )
import Types (  SynTree (And ,Not, Or,Impl,Equi,Leaf))
import Parsing ( normParse )
import Data.Char (isLetter)
import qualified Control.Exception as Exc (evaluate)
import Data.Maybe ( fromJust, isNothing,fromMaybe )
import Data.List.Extra (nubOrd)
import Generate (genSynTree, rangeDepthForNodes, maxLeavesForNodes, maxNodesForDepth)
import Print (display)

nodenum :: SynTree c -> Int
nodenum (Not a) = 1+nodenum a
nodenum (Leaf a)= 1
nodenum (And a b) = 1+nodenum a+nodenum b
nodenum (Or a b) = 1+nodenum a+nodenum b
nodenum (Impl a b) = 1+nodenum a+nodenum b
nodenum (Equi a b) = 1+nodenum a+nodenum b

treedepth :: SynTree c -> Int
treedepth (Not a) = 1 + treedepth a
treedepth (Leaf a)= 1
treedepth (And a b) = 1 + maximum [treedepth a,treedepth b]
treedepth (Or a b) = 1 + maximum [treedepth a,treedepth b]
treedepth (Impl a b) = 1 + maximum [treedepth a,treedepth b]
treedepth (Equi a b) = 1 + maximum [treedepth a,treedepth b]

catchstr :: SynTree Char -> String
catchstr (Not a) = catchstr a
catchstr (Leaf a)= a:""
catchstr (And a b) = catchstr a ++ catchstr b
catchstr (Or a b) =  catchstr a ++ catchstr b
catchstr (Impl a b) =  catchstr a ++ catchstr b
catchstr (Equi a b) =  catchstr a ++ catchstr b


invalidBoundsSyntr :: Gen ((Int, Int), Int, String, Int)
invalidBoundsSyntr = do
 validChars <- sublistOf ['A'..'Z']
 minnode <- chooseInt (2,100)
 maxnode <- chooseInt (1,minnode-1)
 maxdepth <- chooseInt (fst (rangeDepthForNodes minnode), maxnode)
 pure ((minnode, maxnode), maxdepth, validChars, length validChars)

validBoundsSyntr :: Gen ((Int, Int), Int, String, Int, Bool)
validBoundsSyntr = do
 booer <- elements [True,False]
 validChars <- sublistOf ['A'..'Z']
 minnode <- chooseInt (1,60)
 maxnode <- chooseInt (minnode,60)
 maxdepth <- chooseInt (fst (rangeDepthForNodes minnode), maxnode)
 useChars <- chooseInt (0, maxLeavesForNodes (min maxnode (maxNodesForDepth maxdepth)))
 pure ((minnode, maxnode), maxdepth, validChars, min useChars (length validChars), booer)


validBoundsSyntr2 :: Gen ((Int, Int), Int, String, Int, Bool)
validBoundsSyntr2 = do
 booer <- elements [True,False]
 validChars <- sublistOf ['A'..'Z']
 minnode <- chooseInt (1,60)
 maxnode <- chooseInt (minnode,60)
 useChars <- chooseInt (0, maxLeavesForNodes maxnode)
 pure ((minnode, maxnode), maxnode , validChars, min useChars (length validChars), booer)

spec :: Spec
spec = describe "genSyntaxTree" $ do
        it "should generate a random SyntaxTree from the given parament and can be parsed by normParse" $
            forAll validBoundsSyntr $ \((minnode,maxnode), maxdepth ,validChars,minuse,addoper)->forAll (fromJust $ genSynTree (minnode,maxnode) maxdepth validChars minuse addoper)  $ \synTree -> normParse (display synTree)==Right synTree
        it "should reject invalid bounds" $
            forAll invalidBoundsSyntr $ \((minnode,maxnode), maxdepth, validChars, minuse) -> isNothing (genSynTree (minnode,maxnode) maxdepth validChars minuse True)
        it "should generate a random SyntaxTree from the given parament and in the node area" $
            forAll validBoundsSyntr $ \((minnode,maxnode), maxdepth ,validChars,minuse,addoper)->forAll (fromJust $ genSynTree (minnode,maxnode) maxdepth validChars minuse addoper)  $ \synTree -> nodenum synTree>=minnode &&nodenum synTree<=maxnode
        it "should generate a random SyntaxTree from the given parament and not deeper than the maxdepth" $
            forAll validBoundsSyntr $ \((minnode,maxnode), maxdepth ,validChars,minuse,addoper)->forAll (fromJust $ genSynTree (minnode,maxnode) maxdepth validChars  minuse addoper)  $ \synTree -> treedepth synTree<= maxdepth
        it "should generate a random SyntaxTree from the given parament and use as many chars as it must use" $
            forAll validBoundsSyntr2 $ \((minnode, maxnode),maxdepth, validChars, minuse, addoper) -> forAll (fromJust $ genSynTree (minnode, maxnode) maxdepth validChars minuse addoper) $ \synTree -> length (nubOrd (catchstr synTree)) >= minuse
