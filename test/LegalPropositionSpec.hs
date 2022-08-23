{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalPropositionSpec where

import Data.Set (toList, Set, singleton)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Data.List (intercalate, (\\))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, forAll, suchThat)

import Tasks.LegalProposition.Config (LegalPropositionConfig (..), LegalPropositionInst(..), checkLegalPropositionConfig, defaultLegalPropositionConfig)
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.LegalProposition.PrintBracket (bracketDisplay,)
import Tasks.LegalProposition.Quiz (generateLegalPropositionInst, feedback)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Parsing (formulaParse)
import Tasks.SubTree.Parsing (subFormulasStringParse)
import Trees.Generate (genSynTree)
import Trees.Helpers (maxLeavesForNodes)
import SynTreeSpec (validBoundsSyntr)
import Trees.Print (display)
import TestHelpers (deleteBrackets, deleteSpaces)

validBoundsLegalProposition :: Gen LegalPropositionConfig
validBoundsLegalProposition = do
    syntaxTreeConfig@SynTreeConfig {..}  <- validBoundsSyntr `suchThat` ((3 <=) . minNodes)
    let leaves = maxLeavesForNodes maxNodes
    formulas <- choose (1, min 15 ( if useImplEqui then (4 :: Integer) else (2 :: Integer) ^ (maxNodes - leaves)))
    illegals <- choose (0, formulas)
    bracketFormulas <- choose (0, formulas - illegals)
    return $ LegalPropositionConfig
        {
            syntaxTreeConfig
            , formulas
            , illegals
            , bracketFormulas
        }

invalidBoundsLegalProposition :: Gen LegalPropositionConfig
invalidBoundsLegalProposition = do
    syntaxTreeConfig <- validBoundsSyntr
    formulas <- choose (1, 19)
    illegals <- choose (formulas + 1, 20)
    bracketFormulas <- choose (1, 20)
    return $ LegalPropositionConfig
        {
            syntaxTreeConfig
            , formulas
            , illegals
            , bracketFormulas
        }

illegaltest :: [Int] -> [String] -> Bool
illegaltest xs strings = and (map (\ x -> isLeft (formulaParse (strings !! (x - 1)))) xs)

legaltest :: [Int] -> [String] -> Bool
legaltest xs strings = and (map (\ x -> isRight (formulaParse (strings !! (x - 1)))) xs)

transferSetIntToString :: Set Int -> String
transferSetIntToString setInt ="{" ++ intercalate "," (map show (toList setInt)) ++ "}"

spec :: Spec
spec = do
    describe "checkLegalPropositionConfig" $ do
        it "should reject invalid bounds" $
            forAll invalidBoundsLegalProposition (isJust . checkLegalPropositionConfig)
        it "should accept the default config" $
            isNothing (checkLegalPropositionConfig defaultLegalPropositionConfig)
        it "should accept valid bounds" $
            forAll validBoundsLegalProposition (isNothing . checkLegalPropositionConfig)
    describe "illegalDisplay" $ do
        it "at least creates actual formula symbols" $
            forAll validBoundsSyntr $ \SynTreeConfig {..} ->
                forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree ->
                    forAll (deleteSpaces <$> illegalDisplay synTree) $ \str -> subFormulasStringParse ("{" ++ str ++ "}") == Right (singleton str)
        it "the String after illegalDisplay can not parse " $
            forAll validBoundsSyntr $ \SynTreeConfig {..} ->
                forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree ->
                    forAll (illegalDisplay synTree) $ \str -> isLeft (formulaParse str)
    describe "bracket display" $ do
        it "the String after bracketDisplay just add a bracket " $
            forAll validBoundsSyntr $ \SynTreeConfig {..} ->
                forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree ->
                    forAll (bracketDisplay synTree) $ \str -> length str == length (display synTree) + 2
        it "the String can be parsed by formulaParse" $
            forAll validBoundsSyntr $ \SynTreeConfig {..} ->
                forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree ->
                    forAll (bracketDisplay synTree) $ \str -> formulaParse str == Right synTree
        it "the String remove all brackets should same with display remove all brackets" $
            forAll validBoundsSyntr $ \SynTreeConfig {..} ->
                forAll (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree ->
                    forAll (bracketDisplay synTree) $ \str -> deleteBrackets str == deleteBrackets (display synTree)
    describe "generateLegalPropositionInst" $ do
        it "the generateLegalPropositionInst should generate expected illegal number" $
            forAll validBoundsLegalProposition $ \lPConfig ->
                forAll (generateLegalPropositionInst lPConfig) $ \LegalPropositionInst{..} -> illegaltest (toList serialsOfWrong) pseudoFormulas
        it "the generateLegalPropositionInst should generate expected legal number" $
            forAll validBoundsLegalProposition $ \lPConfig@LegalPropositionConfig{..} ->
                forAll (generateLegalPropositionInst lPConfig) $ \LegalPropositionInst{..} -> legaltest ([1.. (fromIntegral formulas)] \\ toList serialsOfWrong) pseudoFormulas
        it "the feedback designed for Instance can works good" $
            forAll validBoundsLegalProposition $ \lPConfig ->
                forAll (generateLegalPropositionInst lPConfig) $ \lPInst@LegalPropositionInst{..} -> feedback lPInst (transferSetIntToString serialsOfWrong)
