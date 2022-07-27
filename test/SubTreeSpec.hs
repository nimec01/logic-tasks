{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module SubTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (Gen, choose, forAll, elements, suchThat)
import Data.Set (size, toList, map)

import Tasks.SubTree.Config (SubTreeConfig(..), checkSubTreeConfig, defaultSubTreeConfig)
import Types (allNotLeafSubTrees)
import Generate (maxLeavesForNodes, noSameSubTree, genSynTreeSubTreeExc)
import Tasks.SynTree.Config (SynTreeConfig(..),)
import Data.Maybe (isJust, isNothing)
import Print (displaySubTrees, display)
import Parsing (subFormulasStringParse, subTreeStringParse)
import SynTreeSpec (invalidBoundsSyntr, validBoundsSyntr)

validBoundsSubTree :: Gen SubTreeConfig
validBoundsSubTree = do
    allowDupelTree <- elements [True,False]
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSyntr `suchThat` ((1<) . minNodes)
    minSubTrees <- choose (1, minNodes - maxLeavesForNodes minNodes)
    return $ SubTreeConfig
      {
        syntaxTreeConfig
      , allowDupelTree
      , minSubTrees
      }

invalidBoundsSubTree :: Gen SubTreeConfig
invalidBoundsSubTree = do
    allowDupelTree <- elements [True,False]
    syntaxTreeConfig@SynTreeConfig {..} <- invalidBoundsSyntr
    minSubTrees <- choose (minNodes - maxLeavesForNodes minNodes + 1, 100)
    return $ SubTreeConfig
      {
        syntaxTreeConfig
      , allowDupelTree
      , minSubTrees
      }

spec :: Spec
spec = do
    describe "checkSubTreeConfig" $ do
        it "should reject invalid bounde in checkSubTreeConfig" $
            forAll invalidBoundsSubTree (isJust . checkSubTreeConfig)
        it "should accept the default config" $
            isNothing (checkSubTreeConfig defaultSubTreeConfig)
        it "should accept valid bounds" $
            forAll validBoundsSubTree (isNothing . checkSubTreeConfig)
    describe "genSynTreeSubTreeExc" $ do
        it "parse should works well" $
            forAll validBoundsSubTree $ \SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui allowDupelTree minSubTrees) $
                \synTree ->subTreeStringParse (displaySubTrees (toList (allNotLeafSubTrees synTree))) == Right (allNotLeafSubTrees synTree)
        it "parse should works well" $
            forAll validBoundsSubTree $ \SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui allowDupelTree minSubTrees) $
                \synTree -> subFormulasStringParse (filter (/= ' ') (displaySubTrees (toList (allNotLeafSubTrees synTree)))) == Right (Data.Set.map display (allNotLeafSubTrees synTree))
        it "it should generate not less Syntax Sub tree number it required as excepted" $
            forAll validBoundsSubTree $ \SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui allowDupelTree minSubTrees) $
                \synTree -> fromIntegral (size (allNotLeafSubTrees synTree)) >= minSubTrees
        it "it should generate the Syntax tree without Duple tree when don't allow Duple Tree" $
            forAll validBoundsSubTree $ \SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
            -> forAll (genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui allowDupelTree minSubTrees) $
                \synTree -> allowDupelTree || noSameSubTree synTree
