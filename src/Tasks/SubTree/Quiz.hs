{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    feedback,
    generateSubTreeInst,
    genSynTreeSubTreeExc,
    genSubTreeInst
) where

import Test.QuickCheck (generate, Gen, suchThat)
import Trees.Generate (genSynTree)
import Data.Set (Set, isSubsetOf, size, map)

import Tasks.SubTree.Parsing (subFormulasStringParse, subTreeStringParse)
import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), SubTreeInst)
import Trees.Print (display)
import Trees.Types (SynTree, Op)
import Trees.Helpers (allNotLeafSubTrees, noSameSubTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Text.Parsec (ParseError)

genSubTreeInst :: SubTreeConfig -> IO SubTreeInst
genSubTreeInst subTreeConfig = generate (generateSubTreeInst subTreeConfig)

generateSubTreeInst :: SubTreeConfig -> Gen SubTreeInst
generateSubTreeInst SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui allowDupelTree maxConsecutiveNegations minSubTrees
    let correctTrees = allNotLeafSubTrees tree
    return $ SubTreeInst
      { minInputTrees = minSubTrees
      , formula = display tree
      , correctTrees
      , correctFormulas = Data.Set.map display correctTrees
      }

genSynTreeSubTreeExc :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Bool -> Integer -> Integer -> Gen (SynTree Op Char)
genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui allowDupelTree maxConsecutiveNegations minSubTrees =
    let
        syntaxTree = genSynTree (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui maxConsecutiveNegations
    in
        syntaxTree `suchThat` \synTree -> (allowDupelTree || noSameSubTree synTree) && fromIntegral (size (allNotLeafSubTrees synTree)) >= minSubTrees

feedback :: SubTreeInst -> String -> Bool
feedback SubTreeInst {correctFormulas, correctTrees, minInputTrees} input = judgeInput (subTreeStringParse input) (subFormulasStringParse (filter (/= ' ') input)) minInputTrees correctFormulas correctTrees

judgeInput :: Either ParseError (Set (SynTree Op Char)) -> Either ParseError (Set String) -> Integer -> Set String -> Set (SynTree Op Char) -> Bool
judgeInput (Right inputTreesSet) (Right inputFormulasSet) minInputTrees correctFormulas correctTrees = inputTreesSet `isSubsetOf` correctTrees && inputFormulasSet `isSubsetOf` correctFormulas && fromIntegral (size inputFormulasSet) >= minInputTrees
judgeInput _ _ _ _ _ = False
