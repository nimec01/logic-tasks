{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module SubTreeSpec where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (Gen, choose, forAll, elements, suchThat)
import Data.Set (size, toList, map)
import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), checkSubTreeConfig, defaultSubTreeConfig)
import Tasks.SubTree.Quiz (generateSubTreeInst, feedback)
import Trees.Types (SynTree, Op)
import Trees.Helpers (maxLeavesForNodes, )
import Tasks.SynTree.Config (SynTreeConfig(..),)
import Data.Maybe (isJust, isNothing)
import Data.List (intercalate)
import Data.Char (isSpace)
import Trees.Print (display)
import Tasks.SubTree.Parsing (subFormulasStringParse, subTreeStringParse)
import SynTreeSpec (validBoundsSyntr)
import Data.List.Extra (isInfixOf )

validBoundsSubTree :: Gen SubTreeConfig
validBoundsSubTree = do
    allowDupelTree <- elements [True,False]
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSyntr `suchThat` ((4<=) . minNodes)
    minSubTrees <- choose (2, minNodes - maxLeavesForNodes minNodes)
    return $ SubTreeConfig
      {
        syntaxTreeConfig
      , allowDupelTree
      , minSubTrees
      }

invalidBoundsSubTree :: Gen SubTreeConfig
invalidBoundsSubTree = do
    allowDupelTree <- elements [True,False]
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSyntr
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
    describe "generateSubTreeInst" $ do
        it "parse should works well" $
            forAll validBoundsSubTree $ \subTreeConfig ->
                forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} -> subTreeStringParse (displaySubTrees (toList correctTrees)) == Right correctTrees
        it "parse should works well" $
            forAll validBoundsSubTree $ \subTreeConfig ->
                forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} -> subFormulasStringParse (filter (not . isSpace) (displaySubTrees (toList correctTrees))) == Right (Data.Set.map (filter (not . isSpace). display) correctTrees)
        it "it should generate not less Syntax Sub tree number it required as excepted" $
            forAll validBoundsSubTree $ \sTconfig@SubTreeConfig {..} ->
                forAll (generateSubTreeInst sTconfig) $ \SubTreeInst{..} -> fromIntegral (size correctFormulas) >= minSubTrees
        it "all subformulas is the sublist of formula" $
            forAll validBoundsSubTree $ \sTconfig@SubTreeConfig {..} ->
                forAll (generateSubTreeInst sTconfig) $ \SubTreeInst{..} -> let correctFormulas' = toList correctFormulas in all (`isInfixOf` formula) correctFormulas'
        it "the correct store in Inst should be accept by feedback" $
            forAll validBoundsSubTree $ \subTreeConfig ->
                forAll (generateSubTreeInst subTreeConfig) $ \subConfig@SubTreeInst{..} ->  feedback subConfig (displaySubTrees $ toList correctTrees)
        it "the correct store in Inst should be accept by feedback, even without spaces" $
            forAll validBoundsSubTree $ \subTreeConfig ->
                forAll (generateSubTreeInst subTreeConfig) $ \subConfig@SubTreeInst{..} ->  feedback subConfig (filter (not . isSpace) . displaySubTrees $ toList correctTrees)

displaySubTrees :: [SynTree Op Char] -> String
displaySubTrees trees = "{" ++ showTrees trees ++ "}"

showTrees :: [SynTree Op Char] -> String
showTrees synTreeList = intercalate ", " (Prelude.map display synTreeList)
