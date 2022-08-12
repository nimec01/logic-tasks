{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    feedback,
    generateSubTreeInst,
) where

import Test.QuickCheck (Gen, suchThat)
import Trees.Generate (genSynTree)
import Data.Set (Set, isSubsetOf, size, map)
import Data.Char (isSpace)

import Tasks.SubTree.Parsing (subFormulasStringParse, subTreeStringParse)
import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), SubTreeInst)
import Trees.Print (display)
import Trees.Types (SynTree, Op)
import Trees.Helpers (allNotLeafSubTrees, noSameSubTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Text.Parsec (ParseError)

generateSubTreeInst :: SubTreeConfig -> Gen SubTreeInst
generateSubTreeInst SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations
      `suchThat` \synTree -> (allowDupelTree || noSameSubTree synTree) && fromIntegral (size (allNotLeafSubTrees synTree)) >= minSubTrees
    let correctTrees = allNotLeafSubTrees tree
    return $ SubTreeInst
      { minInputTrees = minSubTrees
      , formula = display tree
      , correctTrees
      , correctFormulas = Data.Set.map display correctTrees
      }

feedback :: SubTreeInst -> String -> Bool
feedback SubTreeInst {correctFormulas, correctTrees, minInputTrees} input = judgeInput (subTreeStringParse input) (subFormulasStringParse (filter (not . isSpace) input)) minInputTrees correctFormulas correctTrees

judgeInput :: Either ParseError (Set (SynTree Op Char)) -> Either ParseError (Set String) -> Integer -> Set String -> Set (SynTree Op Char) -> Bool
judgeInput (Right inputTreesSet) (Right inputFormulasSet) minInputTrees correctFormulas correctTrees = inputTreesSet `isSubsetOf` correctTrees && inputFormulasSet `isSubsetOf` correctFormulas && fromIntegral (size inputFormulasSet) >= minInputTrees
judgeInput _ _ _ _ _ = False
