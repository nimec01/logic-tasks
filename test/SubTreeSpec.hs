{-# LANGUAGE RecordWildCards #-}

module SubTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (Gen, choose, sublistOf, forAll, elements, suchThat)
import Data.Set (size, toList)

import Tasks.SubTree.Config (SubtreeConfig(..), checkSubTreeConfig, defaultSubtreeConfig)
import Types (allSubtree)
import Generate (maxLeavesForNodes, noSameSubTree, genSynTreeSubtreeExc, maxNodesForDepth, minDepthForNodes)
import Tasks.SynTree.Config (SynTreeConfig(..),)
import Data.Maybe (isJust, isNothing)
import Print (displaySubtree)
import Parsing (subtreeStringParse)

validBoundsSyntr :: Gen SynTreeConfig
validBoundsSyntr = do
  useImplEqui <- elements [True, False]
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNodes <- choose (1, 20)
  maxNodes <- choose (minNodes, 20)
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

validBoundsSubtree :: Gen SubtreeConfig
validBoundsSubtree = do
    useDupelTree <- elements [True,False]
    SynTreeConfig {..} <- validBoundsSyntr
    minSubtreeNum <- choose (1,  min maxNodes (maxNodesForDepth maxDepth))
    return $ SubtreeConfig
      {
        syntaxTreeConfig = SynTreeConfig
          {
            maxNodes = min maxNodes (maxNodesForDepth maxDepth)
          , minNodes = max minNodes (atLeastOccurring * 2 - 1)
          , maxDepth = maxDepth
          , usedLiterals = usedLiterals
          , atLeastOccurring = atLeastOccurring
          , useImplEqui = useImplEqui
          }
      , useDupelTree = useDupelTree
      , minSubtreeNum = minSubtreeNum
      }

invalidBoundsSubtree :: Gen SubtreeConfig
invalidBoundsSubtree = do
    useDupelTree <- elements [True,False]
    SynTreeConfig {..} <- validBoundsSyntr
    minSubtreeNum <- choose (maxNodes + 1, 100)
    return $ SubtreeConfig
      {
        syntaxTreeConfig = SynTreeConfig
          {
            maxNodes = min maxNodes (maxNodesForDepth maxDepth)
          , minNodes = max minNodes (atLeastOccurring * 2 - 1)
          , maxDepth = maxDepth
          , usedLiterals = usedLiterals
          , atLeastOccurring = atLeastOccurring
          , useImplEqui = useImplEqui
          }
      , useDupelTree = useDupelTree
      , minSubtreeNum = minSubtreeNum
      }

invalidBoundsSubtree2 :: Gen SubtreeConfig
invalidBoundsSubtree2 = do
    useDupelTree <- elements [True,False]
    useImplEqui <- elements [True,False]
    minNodes <- choose (2, 20)
    maxNodes <- choose (1, minNodes - 1)
    usedLiterals <- sublistOf ['A'..'Z'] `suchThat` (not . null)
    maxDepth <- choose (minDepthForNodes minNodes, maxNodes)
    useChars <- choose (1, maxLeavesForNodes (min minNodes (maxNodesForDepth maxDepth)))
    minSubtreeNum <- choose (1, min maxNodes (maxNodesForDepth maxDepth))
    let minUse = min useChars (fromIntegral (length usedLiterals))
    return $ SubtreeConfig
      {
        syntaxTreeConfig = SynTreeConfig
          {
            maxNodes = min maxNodes (maxNodesForDepth maxDepth)
          , minNodes = max minNodes (minUse * 2 - 1)
          , maxDepth = maxDepth
          , usedLiterals = usedLiterals
          , atLeastOccurring = minUse
          , useImplEqui = useImplEqui
          }
      , useDupelTree = useDupelTree
      , minSubtreeNum = minSubtreeNum
      }

spec :: Spec
spec = do
    describe "checkSubTreeConfig" $ do
        it "should reject invalid bounds in checkSynTreeConfig" $
            forAll invalidBoundsSubtree2 (isJust . checkSubTreeConfig)
        it "should reject invalid bounde in checkSubTreeConfig" $
            forAll invalidBoundsSubtree (isJust . checkSubTreeConfig)
        it "should accept the default config" $
            isNothing (checkSubTreeConfig defaultSubtreeConfig)
        it "should accept valid bounds" $
            forAll validBoundsSubtree (isNothing . checkSubTreeConfig)
    describe "genSynTreeSubtreeExc" $ do
        it "parse should works well" $
            forAll validBoundsSubtree $ \SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubtreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree minSubtreeNum) $
                \synTree ->subtreeStringParse (displaySubtree (toList (allSubtree synTree))) == Right (allSubtree synTree)
        it "it should generate the same Syntax Sub tree number as excepted when don't allow Duple tree" $
            forAll validBoundsSubtree $ \SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubtreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree minSubtreeNum) $
                \synTree -> size ( allSubtree synTree) >= fromIntegral minSubtreeNum
        it "it should generate the Syntax tree without Duple tree when don't allow Duple Tree" $
            forAll validBoundsSubtree $ \SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubtreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree minSubtreeNum) $
                \synTree -> useDupelTree || noSameSubTree synTree
