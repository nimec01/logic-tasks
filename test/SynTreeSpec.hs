{-# LANGUAGE RecordWildCards #-}

module SynTreeSpec where

import Data.List.Extra (nubOrd)
import Data.Maybe (isJust, isNothing)
import Generate (genSynTree, maxLeavesForNodes, maxNodesForDepth, rangeDepthForNodes)
import Parsing (formulaParse)
import Print (display)
import Tasks.SynTree.Config (SynTreeConfig (..), checkSynTreeConfig, dSynTreeConfig)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, elements, forAll, sublistOf, suchThat)
import Types (SynTree (..), collectLeaves)

nodenum :: SynTree c -> Integer
nodenum (Not a) = 1 + nodenum a
nodenum (Leaf _) = 1
nodenum (And a b) = 1 + nodenum a + nodenum b
nodenum (Or a b) = 1 + nodenum a + nodenum b
nodenum (Impl a b) = 1 + nodenum a + nodenum b
nodenum (Equi a b) = 1 + nodenum a + nodenum b

treedepth :: SynTree c -> Integer
treedepth (Not a) = 1 + treedepth a
treedepth (Leaf _) = 1
treedepth (And a b) = 1 + maximum [treedepth a, treedepth b]
treedepth (Or a b) = 1 + maximum [treedepth a, treedepth b]
treedepth (Impl a b) = 1 + maximum [treedepth a, treedepth b]
treedepth (Equi a b) = 1 + maximum [treedepth a, treedepth b]

invalidBoundsSyntr :: Gen SynTreeConfig
invalidBoundsSyntr = do
  usedLiterals <- sublistOf ['A' .. 'Z']
  minNode <- choose (2, 100)
  maxNode <- choose (1, minNode - 1)
  maxDepth <- choose (fst (rangeDepthForNodes minNode), maxNode)
  return $
    SynTreeConfig
      { maxNode = maxNode,
        minNode = minNode,
        maxDepth = maxDepth,
        usedLiterals = usedLiterals,
        atLeastOccurring = fromIntegral (length usedLiterals),
        useImplEqui = True
      }

validBoundsSyntr :: Gen SynTreeConfig
validBoundsSyntr = do
  booer <- elements [True, False]
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNode <- choose (1, 60)
  maxNode <- choose (minNode, 60)
  maxDepth <- choose (fst (rangeDepthForNodes minNode), maxNode)
  useChars <- choose (1, maxLeavesForNodes (min maxNode (maxNodesForDepth maxDepth)))
  let minUse = min useChars (fromIntegral (length usedLiterals))
  return $
    SynTreeConfig
      { maxNode = min maxNode (maxNodesForDepth maxDepth),
        minNode = max minNode (minUse * 2 - 1),
        maxDepth = maxDepth,
        usedLiterals = usedLiterals,
        atLeastOccurring = minUse,
        useImplEqui = booer
      }

validBoundsSyntr2 :: Gen SynTreeConfig
validBoundsSyntr2 = do
  booer <- elements [True, False]
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNode <- choose (1, 60)
  maxNode <- choose (minNode, 60)
  useChars <- choose (1, maxLeavesForNodes maxNode)
  let minUse = min useChars (fromIntegral (length usedLiterals))
  return $
    SynTreeConfig
      { maxNode = maxNode,
        minNode = max minNode (minUse * 2 - 1),
        maxDepth = maxNode,
        usedLiterals = usedLiterals,
        atLeastOccurring = min useChars (fromIntegral (length usedLiterals)),
        useImplEqui = booer
      }

spec :: Spec
spec = do
  describe "checkSynTreeConfig" $ do
    it "should reject invalid bounds" $
      forAll invalidBoundsSyntr (isJust . checkSynTreeConfig)
    it "should reject a corner case configuration" $
      isJust (checkSynTreeConfig (SynTreeConfig 1 1 2 "A" 1 True))
    it "should accept the default config" $
      isNothing (checkSynTreeConfig dSynTreeConfig)
    it "should accept valid bounds" $
      forAll validBoundsSyntr (isNothing . checkSynTreeConfig)
    it "should accept valid bounds" $
      forAll validBoundsSyntr2 (isNothing . checkSynTreeConfig)
  describe "genSyntaxTree" $ do
    it "should generate a random SyntaxTree from the given parament and can be parsed by formulaParse" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> formulaParse (display synTree) == Right synTree
    it "should generate a random SyntaxTree from the given parament and in the node area" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> nodenum synTree >= minNode && nodenum synTree <= maxNode
    it "should generate a random SyntaxTree from the given parament and not deeper than the maxDepth" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> treedepth synTree <= maxDepth
    it "should generate a random SyntaxTree from the given parament and use as many chars as it must use" $
      forAll validBoundsSyntr2 $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> fromIntegral (length (nubOrd (collectLeaves synTree))) >= atLeastOccurring
