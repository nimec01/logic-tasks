{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module SuperfluousBracketsSpec where

import Tasks.SuperfluousBrackets.Quiz (generateSuperfluousBracketsInst, feedback, genSynTreeSuperfluousBracketsExc)
import Test.QuickCheck (Gen, forAll, choose, suchThat)
import Test.Hspec (Spec, describe, it)
import Tasks.SuperfluousBrackets.Config(SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..), checkSuperfluousBracketsConfig, defaultSuperfluousBracketsConfig)
import Tasks.SynTree.Config (SynTreeConfig(..))
import SynTreeSpec (validBoundsSyntr)
import Data.Maybe (isJust, isNothing)
import Trees.Types (SynTree(..), Op(..))
import Trees.Helpers (numberAllNodes, sameAssociativeOperatorAdjacent, treeNodes)
import Data.Either (isRight)
import Tasks.SuperfluousBrackets.Parsing (superfluousBracketsExcParser)
import Trees.Print (display, simplestDisplay)
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets (superfluousBracketsDisplay, sameAssociativeOperatorAdjacentSerial)
import Trees.Parsing(formulaParse)

validBoundsSuperfluousBrackets :: Gen SuperfluousBracketsConfig
validBoundsSuperfluousBrackets = do
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSyntr `suchThat` ((5<=) . minNodes)
    superfluousBracketPairs <- choose (1, minNodes `div` 2)
    return $ SuperfluousBracketsConfig
        {
          syntaxTreeConfig
        , superfluousBracketPairs
        }

invalidBoundsSuperfluousBrackets :: Gen SuperfluousBracketsConfig
invalidBoundsSuperfluousBrackets = do
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSyntr
    superfluousBracketPairs <- choose (minNodes + 1, 26)
    return $ SuperfluousBracketsConfig
        {
          syntaxTreeConfig
        , superfluousBracketPairs
        }

deleteBrackets :: String  -> String
deleteBrackets = filter (\char -> char /= ')' && char /= '(')

spec :: Spec
spec = do
    describe "checkSuperfluousBracketsConfig" $ do
        it "should reject invalid bounde in checkSuperfluousBracketsConfig" $
            forAll invalidBoundsSuperfluousBrackets (isJust . checkSuperfluousBracketsConfig)
        it "should accept the default config" $
            isNothing (checkSuperfluousBracketsConfig defaultSuperfluousBracketsConfig)
        it "should accept valid bounds" $
            forAll validBoundsSuperfluousBrackets (isNothing . checkSuperfluousBracketsConfig)
    describe "sameAssociativeOperatorAdjacent" $
        it "should return true if two \\/s or two /\\s are Neighboring " $
            sameAssociativeOperatorAdjacent (Unary Not (Binary And (Binary Equi (Leaf 'a') (Leaf 'b')) (Binary And (Leaf 'a') (Leaf 'c'))))
    describe "simplestDisplay and superfluousBracketsDisplay" $ do
        it "simplestDisplay should have less brackets than normal formula" $
            forAll validBoundsSuperfluousBrackets $ \SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} ->
                forAll (genSynTreeSuperfluousBracketsExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $
                    \synTree -> length (sameAssociativeOperatorAdjacentSerial (numberAllNodes synTree) Nothing) * 2 == length (display synTree) - length (simplestDisplay synTree)
        it "after remove all bracket two strings should be same" $
            forAll validBoundsSuperfluousBrackets $ \sBConfig@SuperfluousBracketsConfig {..} ->
                forAll (generateSuperfluousBracketsInst sBConfig) $ \SuperfluousBracketsInst{..} -> deleteBrackets stringWithSuperfluousBrackets == deleteBrackets simplestString
    describe "Parser" $
        it "the Parser can accept all formula generate by simplestShow" $
            forAll validBoundsSuperfluousBrackets $ \sBConfig@SuperfluousBracketsConfig {..} ->
                forAll (generateSuperfluousBracketsInst sBConfig) $ \SuperfluousBracketsInst{..} -> isRight (superfluousBracketsExcParser simplestString)
    describe "validformula" $
        it "the formula Parser can accept when brackets is max number" $
            forAll validBoundsSuperfluousBrackets $ \SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} ->
                forAll (genSynTreeSuperfluousBracketsExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $
                    \synTree -> forAll (superfluousBracketsDisplay synTree (treeNodes synTree + 1)) $ \stringWithSuperfluousBrackets -> isRight (formulaParse stringWithSuperfluousBrackets)
    describe "generateSuperfluousBracketsInst" $ do
        it "the correct store in Inst should be accept by feedback" $
            forAll validBoundsSuperfluousBrackets $ \superfluousBracketsConfig ->
                forAll (generateSuperfluousBracketsInst superfluousBracketsConfig) $ \superfluousBracketsInst@SuperfluousBracketsInst{..} -> feedback superfluousBracketsInst simplestString
        it "the stringWithSuperfluousBrackets should have right number of SuperfluousBrackets" $
            forAll validBoundsSuperfluousBrackets $ \sBConfig@SuperfluousBracketsConfig {..} ->
                forAll (generateSuperfluousBracketsInst sBConfig) $ \SuperfluousBracketsInst{..} -> fromIntegral (length stringWithSuperfluousBrackets - length simplestString) == superfluousBracketPairs * 2
