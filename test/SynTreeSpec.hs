{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SynTreeSpec where

import Data.List.Extra (nubOrd)
import Data.Maybe (isJust, isNothing)
import Generate (genSynTree, maxLeavesForNodes, maxNodesForDepth, minDepthForNodes, consecutiveNegations)
import Parsing (formulaParse)
import Print (display)
import Tasks.SynTree.Config (SynTreeConfig (..), checkSynTreeConfig, defaultSynTreeConfig)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, elements, forAll, sublistOf, suchThat)
import Types (collectLeaves, treeDepth, treeNodes)

validBoundsSyntr :: Gen SynTreeConfig
validBoundsSyntr = do
  useImplEqui <- elements [True, False]
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNodes' <- choose (1, 20)
  maxNodes' <- choose (minNodes', 25)
  maxDepth <- choose (minDepthForNodes minNodes', maxNodes')
  maxConsecutiveNegations <- choose(1, 3)
  let maxNodes = min maxNodes' (maxNodesForDepth maxDepth)
  useChars <- choose (1, maxLeavesForNodes maxNodes)
  let atLeastOccurring = min useChars (fromIntegral (length usedLiterals))
  return $
    SynTreeConfig
      { maxNodes,
        minNodes = max minNodes' (atLeastOccurring * 2 - 1),
        maxDepth,
        usedLiterals,
        atLeastOccurring,
        useImplEqui,
        maxConsecutiveNegations
      }

invalidBoundsSyntr :: Gen SynTreeConfig
invalidBoundsSyntr = do
  usedLiterals <- sublistOf ['A' .. 'Z']
  minNodes <- choose (2, 100)
  maxNodes <- choose (1, minNodes - 1)
  maxDepth <- choose (minDepthForNodes minNodes, maxNodes)
  maxConsecutiveNegations <- choose(1, 3)
  return $
    SynTreeConfig
      { maxNodes,
        minNodes,
        maxDepth,
        usedLiterals,
        atLeastOccurring = fromIntegral (length usedLiterals),
        useImplEqui = True,
        maxConsecutiveNegations
      }

spec :: Spec
spec = do
  describe "checkSynTreeConfig" $ do
    it "should reject invalid bounds" $
      forAll invalidBoundsSyntr (isJust . checkSynTreeConfig)
    it "should reject a corner case configuration" $
      isJust (checkSynTreeConfig (SynTreeConfig 1 1 2 "A" 1 True 1))
    it "should accept the default config" $
      isNothing (checkSynTreeConfig defaultSynTreeConfig)
    it "should accept valid bounds" $
      forAll validBoundsSyntr (isNothing . checkSynTreeConfig)
  describe "genSyntaxTree" $ do
    it "should generate a random SyntaxTree from the given parament and can be parsed by formulaParse" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree -> formulaParse (display synTree) == Right synTree
    it "should generate a random SyntaxTree from the given parament and in the node area" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree -> treeNodes synTree >= minNodes && treeNodes synTree <= maxNodes
    it "should generate a random SyntaxTree from the given parament and not deeper than the maxDepth" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree -> treeDepth synTree <= maxDepth
    it "should generate a random SyntaxTree from the given parament and use as many chars as it must use" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree -> fromIntegral (length (nubOrd (collectLeaves synTree))) >= atLeastOccurring
    it "should generate a random SyntaxTree with limited ConsecutiveNegations" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree -> consecutiveNegations synTree 0 0 <= maxConsecutiveNegations
