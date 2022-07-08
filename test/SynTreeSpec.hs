module SynTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (choose, sublistOf, forAll, Gen, elements)
import Types (SynTree(..), collectLeaves)
import Parsing ( normParse )
-- import Data.Char (isLetter)
-- import qualified Control.Exception as Exc (evaluate)
import Data.Maybe ( fromJust, isNothing)--,fromMaybe
import Data.List.Extra (nubOrd)
import Generate (genSynTree, rangeDepthForNodes, maxLeavesForNodes, maxNodesForDepth)
import Print (display)

nodenum :: SynTree c -> Integer
nodenum (Not a) = 1+nodenum a
nodenum (Leaf _)= 1
nodenum (And a b) = 1+nodenum a+nodenum b
nodenum (Or a b) = 1+nodenum a+nodenum b
nodenum (Impl a b) = 1+nodenum a+nodenum b
nodenum (Equi a b) = 1+nodenum a+nodenum b

treedepth :: SynTree c -> Integer
treedepth (Not a) = 1 + treedepth a
treedepth (Leaf _)= 1
treedepth (And a b) = 1 + maximum [treedepth a,treedepth b]
treedepth (Or a b) = 1 + maximum [treedepth a,treedepth b]
treedepth (Impl a b) = 1 + maximum [treedepth a,treedepth b]
treedepth (Equi a b) = 1 + maximum [treedepth a,treedepth b]


invalidBoundsSyntr :: Gen ((Integer, Integer), Integer, String, Integer)
invalidBoundsSyntr = do
 validChars <- sublistOf ['A'..'Z']
 minnode <- choose (2,100)
 maxnode <- choose (1,minnode-1)
 maxdepth <- choose (fst (rangeDepthForNodes minnode), maxnode)
 return ((minnode, maxnode), maxdepth, validChars, fromIntegral (length validChars))

validBoundsSyntr :: Gen ((Integer, Integer), Integer, String, Integer, Bool)
validBoundsSyntr = do
 booer <- elements [True,False]
 validChars <- sublistOf ['A'..'Z']
 minnode <- choose (1,60)
 maxnode <- choose (minnode,60)
 maxdepth <- choose (fst (rangeDepthForNodes minnode), maxnode)
 useChars <- choose (0, maxLeavesForNodes (min maxnode (maxNodesForDepth maxdepth)))
 return ((minnode, maxnode), maxdepth, validChars, min useChars (fromIntegral (length validChars)), booer)


validBoundsSyntr2 :: Gen ((Integer, Integer), Integer, String, Integer, Bool)
validBoundsSyntr2 = do
 booer <- elements [True,False]
 validChars <- sublistOf ['A'..'Z']
 minnode <- choose (1,60)
 maxnode <- choose (minnode,60)
 useChars <- choose (0, maxLeavesForNodes maxnode)
 return ((minnode, maxnode), maxnode, validChars, min useChars (fromIntegral (length validChars)), booer)

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
            forAll validBoundsSyntr2 $ \((minnode, maxnode),maxdepth, validChars, minuse, addoper) -> forAll (fromJust $ genSynTree (minnode, maxnode) maxdepth validChars minuse addoper) $ \synTree -> fromIntegral (length (nubOrd (collectLeaves synTree))) >= minuse
