{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module SuperfluousBracketsSpec where

import Tasks.SuperfluousBrackets.Quiz (generateSuperfluousBracketsInst, feedback)
import Test.QuickCheck (Gen, forAll, choose, suchThat)
import Generate (sameOperatorAdjacent, genSynTreeSuperfluousBracketsExc)
import Test.Hspec (Spec, describe, it)
import Tasks.SuperfluousBrackets.Config(SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..), checkSuperfluousBracketsConfig, defaultSuperfluousBracketsConfig)
import Tasks.SynTree.Config (SynTreeConfig(..))
import SynTreeSpec (validBoundsSyntr)
import Data.Maybe (isJust, isNothing)
import Types (SynTree(..), Op(..), numberAllNodes)
import Data.Either (isRight)
import Parsing (superfluousBracketsExcParser)
import Print (simplestDisplay, display)
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets (superfluousBracketsDisplay, sameOperatorAdjacentSerial)

validBoundsSuperfluousBrackets :: Gen SuperfluousBracketsConfig
validBoundsSuperfluousBrackets = do
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSyntr `suchThat` ((5<=) . minNodes)
    superfluousBrackets <- choose (1, minNodes)
    return $ SuperfluousBracketsConfig
        {
          syntaxTreeConfig
        , superfluousBrackets = fromIntegral superfluousBrackets
        }

invalidBoundsSuperfluousBrackets :: Gen SuperfluousBracketsConfig
invalidBoundsSuperfluousBrackets = do
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSyntr
    superfluousBrackets <- choose (minNodes + 1, 26)
    return $ SuperfluousBracketsConfig
        {
          syntaxTreeConfig
        , superfluousBrackets = fromIntegral superfluousBrackets
        }

spec :: Spec
spec = do
    describe "checkSuperfluousBracketsConfig" $ do
        it "should reject invalid bounde in checkSuperfluousBracketsConfig" $
            forAll invalidBoundsSuperfluousBrackets (isJust . checkSuperfluousBracketsConfig)
        it "should accept the default config" $
            isNothing (checkSuperfluousBracketsConfig defaultSuperfluousBracketsConfig)
        it "should accept valid bounds" $
            forAll validBoundsSuperfluousBrackets (isNothing . checkSuperfluousBracketsConfig)
    describe "sameOperatorAdjacent" $
        it "should return true if two \\/s or two /\\s are Neighboring " $
            sameOperatorAdjacent (Unary Not (Binary And (Binary Equi (Leaf 'a') (Leaf 'b')) (Binary And (Leaf 'a') (Leaf 'c'))))
    describe "simplestDisplay and superfluousBracketsDisplay" $ do
        it "simplestDisplay should have less brackets than normal formula" $
            forAll validBoundsSuperfluousBrackets $ \SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} ->
                forAll (genSynTreeSuperfluousBracketsExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $
                \synTree -> length (sameOperatorAdjacentSerial (numberAllNodes synTree) Nothing) * 2 == length (display synTree) - length (simplestDisplay synTree)
        it "superfluousBracketsDisplay should have setted addtional brackets than simplest formula" $
            forAll validBoundsSuperfluousBrackets $ \SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} ->
                forAll (genSynTreeSuperfluousBracketsExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $ \synTree ->
                    forAll (superfluousBracketsDisplay synTree superfluousBrackets) $ \bracketsFormula -> length bracketsFormula - length (simplestDisplay synTree) == fromIntegral (superfluousBrackets * 2)
    describe "Parser" $
        it "the Parser can accept all formula generate by simplestShow" $
            forAll validBoundsSuperfluousBrackets $ \SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} ->
               forAll (genSynTreeSuperfluousBracketsExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) $
                    \synTree -> isRight (superfluousBracketsExcParser (simplestDisplay synTree ))
    describe "genSuperfluousBracketsInst" $
        it "the correct store in Inst should be accept by feedback" $
        forAll validBoundsSuperfluousBrackets $ \superfluousBracketsConfig ->
            forAll (generateSuperfluousBracketsInst superfluousBracketsConfig) $ \superfluousBracketsInst@SuperfluousBracketsInst{..} -> feedback superfluousBracketsInst simplestString
