{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module SynTreeSpec where

import Data.List.Extra (nubOrd)
import Data.Maybe (isJust, isNothing)
import Generate (genSynTree, maxLeavesForNodes, maxNodesForDepth, minDepthForNodes)
import Parsing (formulaParse)
import Print (display)
import Tasks.SynTree.Config (SynTreeConfig (..), checkSynTreeConfig, defaultSynTreeConfig)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, elements, forAll, sublistOf, suchThat)
import Types (collectLeaves, treeNodes, treeDepth)

invalidBoundsSyntr :: Gen SynTreeConfig
invalidBoundsSyntr = do
  usedLiterals <- sublistOf ['A' .. 'Z']
  minNodes <- choose (2, 100)
  maxNodes <- choose (1, minNodes - 1)
  maxDepth <- choose (minDepthForNodes minNodes, maxNodes)
  return $
    SynTreeConfig
      { maxNodes,
        minNodes,
        maxDepth,
        usedLiterals,
        atLeastOccurring = fromIntegral (length usedLiterals),
        useImplEqui = True
      }

validBoundsSyntr :: Gen SynTreeConfig
validBoundsSyntr = do
  useImplEqui <- elements [True, False]
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNodes' <- choose (1, 20)
  maxNodes' <- choose (minNodes', 25)
  maxDepth <- choose (minDepthForNodes minNodes', maxNodes')
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
        useImplEqui
      }

validBoundsSyntr2 :: Gen SynTreeConfig
validBoundsSyntr2 = do
  useImplEqui <- elements [True, False]
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNodes' <- choose (1, 20)
  maxNodes <- choose (minNodes', 25)
  useChars <- choose (1, maxLeavesForNodes maxNodes)
  let atLeastOccurring = min useChars (fromIntegral (length usedLiterals))
  return $
    SynTreeConfig
      { maxNodes,
        minNodes = max minNodes' (atLeastOccurring * 2 - 1),
        maxDepth = maxNodes,
        usedLiterals,
        atLeastOccurring,
        useImplEqui
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
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> treeNodes synTree >= minNodes && treeNodes synTree <= maxNodes
    it "should generate a random SyntaxTree from the given parament and not deeper than the maxDepth" $
      forAll validBoundsSyntr $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> treeDepth synTree <= maxDepth
    it "should generate a random SyntaxTree from the given parament and use as many chars as it must use" $
      forAll validBoundsSyntr2 $ \SynTreeConfig {..} ->
        forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree -> fromIntegral (length (nubOrd (collectLeaves synTree))) >= atLeastOccurring
