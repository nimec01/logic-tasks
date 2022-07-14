{-# LANGUAGE RecordWildCards #-}

module SubTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (Gen, choose, sublistOf, forAll, elements, suchThat)
import Data.Set (size)

import Tasks.SubTree.Config (SubtreeConfig(..), checkSubTreeConfig, dSubtreeConfig)
import Types (allSubtree)
import Generate (maxLeavesForNodes, noSameSubTree, genSynTreeSubtreeExc, maxNodesForDepth, rangeDepthForNodes)
import Tasks.SynTree.Config (SynTreeConfig(..),)
import Data.Maybe (isJust, isNothing)
import Print (displaySubtree)
import Parsing (subtreeStringParse)
import Data.Set (toList)

validBoundsSubtree :: Gen SubtreeConfig
validBoundsSubtree = do
    useDupelTree <- elements [True,False]
    useImplEqui <- elements [True,False]
    minNode <- choose (1, 20)
    maxNode <- choose (minNode, 20)
    usedLiterals <- sublistOf ['A'..'Z'] `suchThat` (not . null)
    maxDepth <- choose (fst (rangeDepthForNodes minNode), maxNode)
    useChars <- choose (1, maxLeavesForNodes (min minNode (maxNodesForDepth maxDepth)))
    minSubtreeNum <- choose (1,  min maxNode (maxNodesForDepth maxDepth))
    let minUse = min useChars (fromIntegral (length usedLiterals))
    return $ SubtreeConfig
      {
        syntaxTreeConfig = SynTreeConfig
          {
            maxNode = min maxNode (maxNodesForDepth maxDepth)
          , minNode = max minNode (minUse * 2 - 1)
          , maxDepth = maxDepth
          , usedLiterals = usedLiterals
          , atLeastOccurring = minUse
          , useImplEqui = useImplEqui
          }
      , useDupelTree = useDupelTree
      , minSubtreeNum = minSubtreeNum
      }

invalidBoundsSubtree :: Gen SubtreeConfig
invalidBoundsSubtree = do
    useDupelTree <- elements [True,False]
    useImplEqui <- elements [True,False]
    minNode <- choose (1, 20)
    maxNode <- choose (minNode, 20)
    usedLiterals <- sublistOf ['A'..'Z'] `suchThat` (not . null)
    maxDepth <- choose (fst (rangeDepthForNodes minNode), maxNode)
    useChars <- choose (1, maxLeavesForNodes (min minNode (maxNodesForDepth maxDepth)))
    minSubtreeNum <- choose (maxNode + 1, 100)
    let minUse = min useChars (fromIntegral (length usedLiterals))
    return $ SubtreeConfig
      {
        syntaxTreeConfig = SynTreeConfig
          {
            maxNode = min maxNode (maxNodesForDepth maxDepth)
          , minNode = max minNode (minUse * 2 - 1)
          , maxDepth = maxDepth
          , usedLiterals = usedLiterals
          , atLeastOccurring = minUse
          , useImplEqui = useImplEqui
          }
      , useDupelTree = useDupelTree
      , minSubtreeNum = minSubtreeNum
      }

invalidBoundsSubtree2 :: Gen SubtreeConfig
invalidBoundsSubtree2 = do
    useDupelTree <- elements [True,False]
    useImplEqui <- elements [True,False]
    minNode <- choose (2, 20)
    maxNode <- choose (1, minNode - 1)
    usedLiterals <- sublistOf ['A'..'Z'] `suchThat` (not . null)
    maxDepth <- choose (fst (rangeDepthForNodes minNode), maxNode)
    useChars <- choose (1, maxLeavesForNodes (min minNode (maxNodesForDepth maxDepth)))
    minSubtreeNum <- choose (1,  min maxNode (maxNodesForDepth maxDepth))
    let minUse = min useChars (fromIntegral (length usedLiterals))
    return $ SubtreeConfig
      {
        syntaxTreeConfig = SynTreeConfig
          {
            maxNode = min maxNode (maxNodesForDepth maxDepth)
          , minNode = max minNode (minUse * 2 - 1)
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
            isNothing (checkSubTreeConfig dSubtreeConfig)
        it "should accept valid bounds" $
            forAll validBoundsSubtree (isNothing . checkSubTreeConfig)
    describe "genSynTreeSubtreeExc" $ do
        it "parse should works well" $
            forAll validBoundsSubtree $ \SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubtreeExc (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree minSubtreeNum) $
                \synTree ->subtreeStringParse (displaySubtree (toList (allSubtree synTree))) == Right (allSubtree synTree)
        it "it should generate the same Syntax Sub tree number as excepted when don't allow Duple tree" $
            forAll validBoundsSubtree $ \SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubtreeExc (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree minSubtreeNum) $
                \synTree -> size ( allSubtree synTree) >= fromIntegral minSubtreeNum
        it "it should generate the Syntax tree without Duple tree when don't allow Duple Tree" $
            forAll validBoundsSubtree $ \SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubtreeExc (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree minSubtreeNum) $
                \synTree -> useDupelTree || noSameSubTree synTree
