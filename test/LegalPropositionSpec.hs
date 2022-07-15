{-# LANGUAGE RecordWildCards #-}

module LegalPropositionSpec where

import Test.Hspec (Spec, describe, it)
import Tasks.LegalProposition.Config (LegalPropositionConfig (..), checkLegalPropositionConfig, dLegalPropositionConfig)
import Test.QuickCheck (Gen, choose, forAll, elements, sublistOf, suchThat)
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Data.Maybe (isJust, isNothing)
import Parsing(formulaParse)
import Data.Either (isLeft)
import Generate (rangeDepthForNodes, maxLeavesForNodes, maxNodesForDepth, genSynTree)

validBoundsSyntr :: Gen SynTreeConfig
validBoundsSyntr = do
  booer <- elements [True, False]
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNode <- choose (1, 60)
  maxNode <- choose (minNode, 60)
  maxDepth <- choose (fst (rangeDepthForNodes minNode), maxNode)
  useChars <- choose (1, maxLeavesForNodes (min maxNode (maxNodesForDepth maxDepth)))
  let minUse = min useChars (fromIntegral (length usedLiterals))
  return $
    SynTreeConfig
      { maxNode = min maxNode (maxNodesForDepth maxDepth),
        minNode = max minNode (minUse * 2 - 1),
        maxDepth = maxDepth,
        usedLiterals = usedLiterals,
        atLeastOccurring = minUse,
        useImplEqui = booer
      }

validBoundsLegalProposition :: Gen LegalPropositionConfig
validBoundsLegalProposition = do
    useImplEqui <- elements [True,False]
    minNode <- choose (1, 20)
    maxNode <- choose (minNode, 20)
    usedLiterals <- sublistOf ['A'..'Z'] `suchThat` (not . null)
    maxDepth <- choose (fst (rangeDepthForNodes minNode), maxNode)
    useChars <- choose (1, maxLeavesForNodes (min minNode (maxNodesForDepth maxDepth)))
    formulaNum <- choose (1, 20)
    illegalNum <- choose (1, formulaNum)
    let minUse = min useChars (fromIntegral (length usedLiterals))
    return $ LegalPropositionConfig
        {
            formulaConfig = SynTreeConfig
              {
                maxNode = min maxNode (maxNodesForDepth maxDepth)
              , minNode = max minNode (minUse * 2 - 1)
              , maxDepth = maxDepth
              , usedLiterals = usedLiterals
              , atLeastOccurring = minUse
              , useImplEqui = useImplEqui
              }
            , formulaNum = formulaNum
            , illegalNum = illegalNum
        }

invalidBoundsLegalProposition :: Gen LegalPropositionConfig
invalidBoundsLegalProposition = do
    useImplEqui <- elements [True,False]
    minNode <- choose (1, 20)
    maxNode <- choose (minNode, 20)
    usedLiterals <- sublistOf ['A'..'Z'] `suchThat` (not . null)
    maxDepth <- choose (fst (rangeDepthForNodes minNode), maxNode)
    useChars <- choose (1, maxLeavesForNodes (min minNode (maxNodesForDepth maxDepth)))
    formulaNum <- choose (1, 19)
    illegalNum <- choose (formulaNum + 1, 20)
    let minUse = min useChars (fromIntegral (length usedLiterals))
    return $ LegalPropositionConfig
        {
            formulaConfig = SynTreeConfig
              {
                maxNode = min maxNode (maxNodesForDepth maxDepth)
              , minNode = max minNode (minUse * 2 - 1)
              , maxDepth = maxDepth
              , usedLiterals = usedLiterals
              , atLeastOccurring = minUse
              , useImplEqui = useImplEqui
              }
            , formulaNum = formulaNum
            , illegalNum = illegalNum
        }

spec :: Spec
spec = do
    describe "checkLegalPropositionConfig" $ do
        it "should reject invalid bounds" $
            forAll invalidBoundsLegalProposition (isJust . checkLegalPropositionConfig)
        it "should accept the default config" $
            isNothing (checkLegalPropositionConfig dLegalPropositionConfig)
        it "should accept valid bounds" $
            forAll validBoundsLegalProposition (isNothing . checkLegalPropositionConfig)
    describe "illegalDisplay" $
        it "the String after illegalDisplay can not parse " $
            forAll validBoundsSyntr $ \SynTreeConfig {..} ->
                forAll (genSynTree (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui) $ \synTree ->
                    forAll (illegalDisplay synTree) $ \str -> isLeft (formulaParse str)
