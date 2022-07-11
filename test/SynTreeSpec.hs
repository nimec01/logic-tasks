{-# LANGUAGE RecordWildCards #-}

module SynTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (choose, sublistOf, forAll, Gen, elements, suchThat)
import Types (SynTree(..), collectLeaves)
import Tasks.SynTree.Config (SynTreeConfig(..), checkSynTreeConfig, dSynTreeConfig)
import Parsing ( normParse )
import Data.Maybe (isJust, isNothing)
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


invalidBoundsSyntr :: Gen SynTreeConfig
invalidBoundsSyntr = do
 validChars <- sublistOf ['A'..'Z']
 minnode <- choose (2,100)
 maxnode <- choose (1,minnode-1)
 maxdepth <- choose (fst (rangeDepthForNodes minnode), maxnode)
 return $ SynTreeConfig
   { maxnode = maxnode
   , minnode = minnode
   , maxdepth = maxdepth
   , electliteral = validChars
   , mustcontain = fromIntegral (length validChars)
   , useImplEqui = True
   }

validBoundsSyntr :: Gen SynTreeConfig
validBoundsSyntr = do
 booer <- elements [True,False]
 validChars <- sublistOf ['A'..'Z'] `suchThat` (not . null)
 minnode <- choose (1,60)
 maxnode <- choose (minnode,60)
 maxdepth <- choose (fst (rangeDepthForNodes minnode), maxnode)
 useChars <- choose (1, maxLeavesForNodes (min maxnode (maxNodesForDepth maxdepth)))
 let minuse = min useChars (fromIntegral (length validChars))
 return $ SynTreeConfig
   { maxnode = min maxnode (maxNodesForDepth maxdepth)
   , minnode = max minnode (minuse * 2 - 1)
   , maxdepth = maxdepth
   , electliteral = validChars
   , mustcontain = minuse
   , useImplEqui = booer
   }

validBoundsSyntr2 :: Gen SynTreeConfig
validBoundsSyntr2 = do
 booer <- elements [True,False]
 validChars <- sublistOf ['A'..'Z'] `suchThat` (not . null)
 minnode <- choose (1,60)
 maxnode <- choose (minnode,60)
 useChars <- choose (1, maxLeavesForNodes maxnode)
 let minuse = min useChars (fromIntegral (length validChars))
 return $ SynTreeConfig
   { maxnode = maxnode
   , minnode = max minnode (minuse * 2 - 1)
   , maxdepth = maxnode
   , electliteral = validChars
   , mustcontain = min useChars (fromIntegral (length validChars))
   , useImplEqui = booer
   }

spec :: Spec
spec = do
    describe "checkSynTreeConfig" $ do
        it "should reject invalid bounds" $
            forAll invalidBoundsSyntr (isJust . checkSynTreeConfig)
        it "should accept the default config" $
            isNothing (checkSynTreeConfig dSynTreeConfig)
        it "should accept valid bounds" $
            forAll validBoundsSyntr (isNothing . checkSynTreeConfig)
        it "should accept valid bounds" $
            forAll validBoundsSyntr2 (isNothing . checkSynTreeConfig)
    describe "genSyntaxTree" $ do
        it "should generate a random SyntaxTree from the given parament and can be parsed by normParse" $
            forAll validBoundsSyntr $ \SynTreeConfig{electliteral = validChars, mustcontain = minuse, ..} -> forAll (genSynTree (minnode,maxnode) maxdepth validChars minuse useImplEqui)  $ \synTree -> normParse (display synTree) == Right synTree
        it "should generate a random SyntaxTree from the given parament and in the node area" $
            forAll validBoundsSyntr $ \SynTreeConfig{electliteral = validChars, mustcontain = minuse, ..} -> forAll (genSynTree (minnode,maxnode) maxdepth validChars minuse useImplEqui)  $ \synTree -> nodenum synTree >= minnode && nodenum synTree <= maxnode
        it "should generate a random SyntaxTree from the given parament and not deeper than the maxdepth" $
            forAll validBoundsSyntr $ \SynTreeConfig{electliteral = validChars, mustcontain = minuse, ..} -> forAll (genSynTree (minnode,maxnode) maxdepth validChars minuse useImplEqui) $ \synTree -> treedepth synTree <= maxdepth
        it "should generate a random SyntaxTree from the given parament and use as many chars as it must use" $
            forAll validBoundsSyntr2 $ \SynTreeConfig{electliteral = validChars, mustcontain = minuse, ..} -> forAll (genSynTree (minnode, maxnode) maxdepth validChars minuse useImplEqui) $ \synTree -> fromIntegral (length (nubOrd (collectLeaves synTree))) >= minuse
