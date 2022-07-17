{-# LANGUAGE RecordWildCards #-}

module SynTreeSpec where

import Data.List.Extra (nubOrd)
import Data.Maybe (isJust, isNothing)
import Generate (genSynTree, maxLeavesForNodes, maxNodesForDepth, minDepthForNodes)
import Parsing (formulaParse)
import Print (display)
import Tasks.SynTree.Config (SynTreeConfig (..), checkSynTreeConfig, defaultSynTreeConfig)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, elements, forAll, sublistOf, suchThat)
import Types (SynTree (..), collectLeaves, treeNodeNum)

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
  minNodes <- choose (2, 100)
  maxNodes <- choose (1, minNodes - 1)
  maxDepth <- choose (minDepthForNodes minNodes, maxNodes)
  return $
    SynTreeConfig
      { maxNodes = maxNodes,
        minNodes = minNodes,
        maxDepth = maxDepth,
        usedLiterals = usedLiterals,
        atLeastOccurring = fromIntegral (length usedLiterals),
        useImplEqui = True
      }

validBoundsSyntr :: Gen SynTreeConfig
validBoundsSyntr = do
  useImplEqui <- elements [True, False]
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNodes <- choose (1, 60)
  maxNodes <- choose (minNodes, 60)
  maxDepth <- choose (minDepthForNodes minNodes, maxNodes)
  useChars <- choose (1, maxLeavesForNodes (min maxNodes (maxNodesForDepth maxDepth)))
  let minUse = min useChars (fromIntegral (length usedLiterals))
  return $
    SynTreeConfig
      { maxNodes = min maxNodes (maxNodesForDepth maxDepth),
        minNodes = max minNodes (minUse * 2 - 1),
        maxDepth = maxDepth,
        usedLiterals = usedLiterals,
        atLeastOccurring = minUse,
        useImplEqui = useImplEqui
      }

validBoundsSyntr2 :: Gen SynTreeConfig
validBoundsSyntr2 = do
  useImplEqui <- elements [True, False]
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNodes <- choose (1, 60)
  maxNodes <- choose (minNodes, 60)
  useChars <- choose (1, maxLeavesForNodes maxNodes)
  let minUse = min useChars (fromIntegral (length usedLiterals))
  return $
    SynTreeConfig
      { maxNodes = maxNodes,
        minNodes = max minNodes (minUse * 2 - 1),
        maxDepth = maxNodes,
        usedLiterals = usedLiterals,
        atLeastOccurring = min useChars (fromIntegral (length usedLiterals)),
        useImplEqui = useImplEqui
      }

spec :: Spec
spec = do
  describe "checkSynTreeConfig" $ do
    it "should reject invalid bounds" $
      forAll invalidBoundsSyntr (isJust . checkSynTreeConfig)
    it "should reject a corner case configuration" $
      isJust (checkSynTreeConfig (SynTreeConfig 1 1 2 "A" 1 True))
    it "should accept the default config" $
      isNothing (checkSynTreeConfig defaultSynTreeConfig)
    it "should accept valid bounds" $
      forAll validBoundsSyntr (isNothing . checkSynTreeConfig)
    it "should accept valid bounds" $
      forAll validBoundsSyntr2 (isNothing . checkSynTreeConfig)
  describe "genSyntaxTree" $ do
    it "should generate a random SyntaxTree from the given parament and can be parsed by formulaParse" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> formulaParse (display synTree) == Right synTree
    it "should generate a random SyntaxTree from the given parament and in the node area" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> treeNodeNum synTree >= fromIntegral minNodes && treeNodeNum synTree <= fromIntegral maxNodes
    it "should generate a random SyntaxTree from the given parament and not deeper than the maxDepth" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> treedepth synTree <= maxDepth
    it "should generate a random SyntaxTree from the given parament and use as many chars as it must use" $
      forAll validBoundsSyntr2 $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> fromIntegral (length (nubOrd (collectLeaves synTree))) >= atLeastOccurring
