{-# LANGUAGE RecordWildCards #-}

module SubTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (Gen, choose, sublistOf, forAll, elements, suchThat)
import Data.Set (size)

import Tasks.SubTree.Config (SubtreeConfig(..))
import Types (allSubtree)
import Generate (maxLeavesForNodes, genSynTreeSubtreeExc, maxNodesForDepth, rangeDepthForNodes)

validBoundsSubtree :: Gen SubtreeConfig
validBoundsSubtree = do
    useImplEqui <- elements [True,False]
    -- useDupelTree <- elements [True,False]
    minNode <- choose (1,20)
    maxNode <- choose (minNode,20)
    subtreeNum <- choose (minNode, minNode)--如果是False必须和minNode相等
    usedLiterals <- sublistOf ['A'..'Z'] `suchThat` (\str ->fromIntegral ( maxLeavesForNodes subtreeNum) <= length str)
    maxDepth <- choose (fst (rangeDepthForNodes minNode), minNode)
    useChars <- choose (1, maxLeavesForNodes (min minNode (maxNodesForDepth maxDepth)))
    let minUse = min useChars (fromIntegral (length usedLiterals))
    return $ SubtreeConfig
      {
        maxNode = min maxNode (maxNodesForDepth maxDepth)
      , minNode = max minNode (minUse * 2 - 1)
      , maxDepth = maxDepth
      , usedLiterals = usedLiterals
      , atLeastOccurring = minUse
      , useImplEqui = useImplEqui
      , useDupelTree = False
      , subtreeNum = subtreeNum
      }

validBoundsSubtreeDup :: Gen SubtreeConfig
validBoundsSubtreeDup = do
    useImplEqui <- elements [True,False]  -- useDupelTree <- elements [True,False]
    minNode <- choose (1,10)
    maxNode <- choose (minNode,10)
    subtreeNum <- choose (minNode, maxNode)
    usedLiterals <- sublistOf ['A'..'Z']   -- `suchThat` (\str ->fromInteger ( maxLeavesForNodes subtreeNum) <= length str)
    maxDepth <- choose (fst (rangeDepthForNodes maxNode), maxNode)
    useChars <- choose (1, maxLeavesForNodes (min minNode (maxNodesForDepth maxDepth)))
    let minUse = min useChars (fromIntegral (length usedLiterals))
    return $ SubtreeConfig
      {
        maxNode = min maxNode (maxNodesForDepth maxDepth)
      , minNode = max minNode (minUse * 2 - 1)
      , maxDepth = maxDepth
      , usedLiterals = usedLiterals
      , atLeastOccurring = minUse
      , useImplEqui = useImplEqui
      , useDupelTree = True
      , subtreeNum = subtreeNum
      }

spec :: Spec
spec = do
    describe "genSynTreeSubtreeExc" $
        it "it should generate the same Syntax Sub tree number as excepted when don't allow Duple tree" $
            forAll validBoundsSubtree $ \SubtreeConfig {..}
            -> forAll (genSynTreeSubtreeExc (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree subtreeNum) $
                \synTree -> size ( allSubtree synTree) == fromIntegral subtreeNum
    describe "genSynTreeSubtreeExc" $
        it "it should generate the same Syntax Sub tree number as excepted when allow Duple Tree" $
            forAll validBoundsSubtreeDup $ \SubtreeConfig {..}
            -> forAll (genSynTreeSubtreeExc (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree subtreeNum) $
                \synTree -> size ( allSubtree synTree) == fromIntegral subtreeNum