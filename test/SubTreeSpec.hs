{-# LANGUAGE RecordWildCards #-}

module SubTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (Gen, choose, sublistOf, forAll, elements, suchThat)

import Tasks.SubTree.Config (SubtreeConfig(..))
import Types (allSubtre)
import Generate (maxLeavesForNodes, genSynTreeSubtreeExc, maxNodesForDepth, rangeDepthForNodes)
import Data.Set (size)

validBoundsSubtree :: Gen SubtreeConfig --
validBoundsSubtree = do
    addoper <- elements [True,False]
    -- useDupTree <- elements [True,False]
    minnode <- choose (1,20)
    maxnode <- choose (minnode,20)
    subtreeNub <- choose (minnode, minnode)--如果是False必须和minnode相等
    validChars <- sublistOf ['A'..'Z'] `suchThat` (\str ->fromInteger ( maxLeavesForNodes subtreeNub) <= length str)
    maxdepth <- choose (fst (rangeDepthForNodes minnode), minnode)
    useChars <- choose (1, maxLeavesForNodes (min minnode (maxNodesForDepth maxdepth)))
    return $ SubtreeConfig
      {
        maxnode = maxnode
      , minnode = minnode
      , maxdepth = maxdepth
      , electliteral = validChars
      , mustcontain = min useChars (fromIntegral (length validChars))
      , addoper = addoper
      , useDupTree = False
      , subtreeNub = subtreeNub
      }

validBoundsSubtreeDup :: Gen SubtreeConfig --
validBoundsSubtreeDup = do
    addoper <- elements [True,False]
    -- useDupTree <- elements [True,False]
    minnode <- choose (1,10)
    maxnode <- choose (minnode,10)
    subtreeNub <- choose (minnode, maxnode)
    validChars <- sublistOf ['A'..'Z'] -- `suchThat` (\str ->fromInteger ( maxLeavesForNodes subtreeNub) <= length str)
    maxdepth <- choose (fst (rangeDepthForNodes maxnode), maxnode)
    useChars <- choose (1, maxLeavesForNodes (min minnode (maxNodesForDepth maxdepth)))
    return $ SubtreeConfig
      {
        maxnode = maxnode
      , minnode = minnode
      , maxdepth = maxdepth
      , electliteral = validChars
      , mustcontain = min useChars (fromIntegral (length validChars))
      , addoper = addoper
      , useDupTree = True
      , subtreeNub = subtreeNub
      }

-- genSynTreeSubtreeExc (minnode, maxnode) maxdepth lits minuse addoper useDupTree subtreeNum = genSynTree (minnode, maxnode) maxdepth lits minuse addoper `suchThat` \synTree -> judgeDupTree synTree == useDupTree && size (allSubtre synTree) == fromIntegral subtreeNum
spec :: Spec
spec = do
    describe "genSynTreeSubtreeExc" $ do
        it "it should generate the same Syntax Sub tree number as excepted when don't allow Duple tree" $
            forAll validBoundsSubtree $ \SubtreeConfig {electliteral = validChars, mustcontain = minuse, ..} -> forAll (genSynTreeSubtreeExc (minnode, maxnode) maxdepth validChars minuse addoper useDupTree subtreeNub) $ \synTree -> size ( allSubtre synTree) == fromIntegral subtreeNub
    describe "genSynTreeSubtreeExc" $ do
        it "it should generate the same Syntax Sub tree number as excepted when allow Duple Tree" $
            forAll validBoundsSubtreeDup $ \SubtreeConfig {electliteral = validChars, mustcontain = minuse, ..} -> forAll (genSynTreeSubtreeExc (minnode, maxnode) maxdepth validChars minuse addoper useDupTree subtreeNub) $ \synTree -> size ( allSubtre synTree) == fromIntegral subtreeNub