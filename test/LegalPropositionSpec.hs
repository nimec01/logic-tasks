{-# LANGUAGE RecordWildCards #-}

module LegalPropositionSpec where

import Test.Hspec (Spec, describe, it)
import Tasks.LegalProposition.Config (LegalPropositionConfig (..), checkLegalPropositionConfig, defaultLegalPropositionConfig)
import Test.QuickCheck (Gen, choose, forAll, elements, sublistOf, suchThat)
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Data.Maybe (isJust, isNothing)
import Parsing(formulaParse)
import Data.Either (isLeft)
import Generate (minDepthForNodes, maxLeavesForNodes, maxNodesForDepth, genSynTree)

validBoundsSyntr :: Gen SynTreeConfig
validBoundsSyntr = do
  booer <- elements [True, False]
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
        useImplEqui = booer
      }

validBoundsLegalProposition :: Gen LegalPropositionConfig
validBoundsLegalProposition = do
    synTreeConfig <- validBoundsSyntr
    formulaNum <- choose (1, 20)
    illegalNum <- choose (1, formulaNum)
    return $ LegalPropositionConfig
        {
            formulaConfig = synTreeConfig
            , formulaNum = formulaNum
            , illegalNum = illegalNum
        }

invalidBoundsLegalProposition :: Gen LegalPropositionConfig
invalidBoundsLegalProposition = do
    synTreeConfig <- validBoundsSyntr
    formulaNum <- choose (1, 19)
    illegalNum <- choose (formulaNum + 1, 20)
    return $ LegalPropositionConfig
        {
            formulaConfig = synTreeConfig
            , formulaNum = formulaNum
            , illegalNum = illegalNum
        }

spec :: Spec
spec = do
    describe "checkLegalPropositionConfig" $ do
        it "should reject invalid bounds" $
            forAll invalidBoundsLegalProposition (isJust . checkLegalPropositionConfig)
        it "should accept the default config" $
            isNothing (checkLegalPropositionConfig defaultLegalPropositionConfig)
        it "should accept valid bounds" $
            forAll validBoundsLegalProposition (isNothing . checkLegalPropositionConfig)
    describe "illegalDisplay" $
        it "the String after illegalDisplay can not parse " $
            forAll validBoundsSyntr $ \SynTreeConfig {..} ->
                forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree ->
                    forAll (illegalDisplay synTree) $ \str -> isLeft (formulaParse str)
