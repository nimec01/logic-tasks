{-# LANGUAGE RecordWildCards #-}

module SubTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (Gen, choose, sublistOf, forAll, elements, suchThat)
import Data.Set (size)

import Tasks.SubTree.Config (SubtreeConfig(..))
import Types (allSubtree)
import Generate (maxLeavesForNodes, noSameSubTree, genSynTreeSubtreeExc, maxNodesForDepth, rangeDepthForNodes)
import Tasks.SynTree.Config (SynTreeConfig(..))

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

spec :: Spec
spec = do
    describe "genSynTreeSubtreeExc" $
        it "it should generate the same Syntax Sub tree number as excepted when don't allow Duple tree" $
            forAll validBoundsSubtree $ \SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubtreeExc (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree minSubtreeNum) $
                \synTree -> size ( allSubtree synTree) >= fromIntegral minSubtreeNum
    describe "genSynTreeSubtreeExc" $
        it "it should generate the Syntax tree without Duple tree when don't allow Duple Tree" $
            forAll validBoundsSubtree $ \SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubtreeExc (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree minSubtreeNum) $
                \synTree -> useDupelTree || noSameSubTree synTree
