{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalPropositionSpec where

import Test.Hspec (Spec, describe, it)
import Tasks.LegalProposition.Config (LegalPropositionConfig (..), checkLegalPropositionConfig, defaultLegalPropositionConfig)
import Test.QuickCheck (Gen, choose, forAll)
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Data.Maybe (isJust, isNothing)
import Parsing(formulaParse)
import Data.Either (isLeft)
import Generate (genSynTree, maxLeavesForNodes)
import SynTreeSpec (validBoundsSyntr)

validBoundsLegalProposition :: Gen LegalPropositionConfig
validBoundsLegalProposition = do
    synTreeConfig@SynTreeConfig {..}  <- validBoundsSyntr
    let leaves = maxLeavesForNodes maxNodes
    formulas <- choose (1, max 1 ((maxNodes - leaves) ^ if useImplEqui then (4 :: Integer) else (2 :: Integer)) * (leaves * fromIntegral (length usedLiterals)))
    illegals <- choose (0, formulas)
    return $ LegalPropositionConfig
        {
            formulaConfig = synTreeConfig
            , formulas
            , illegals
        }

invalidBoundsLegalProposition :: Gen LegalPropositionConfig
invalidBoundsLegalProposition = do
    synTreeConfig <- validBoundsSyntr
    formulas <- choose (1, 19)
    illegals <- choose (formulas + 1, 20)
    return $ LegalPropositionConfig
        {
            formulaConfig = synTreeConfig
            , formulas
            , illegals
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
