{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    feedback,
    generateSubTreeInst,
    genSubTreeInst
) where

import Test.QuickCheck (generate, Gen)
import Generate (genSynTreeSubTreeExc)
import Data.Set (Set, isSubsetOf, size, map)

import Parsing (subFormulasStringParse, subTreeStringParse)
import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), SubTreeInst)
import Print (display)
import Types (allNotLeafSubTrees, SynTree, Op)
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

feedback :: SubTreeInst -> String -> Bool
feedback SubTreeInst {correctFormulas, correctTrees, minInputTrees} input = judgeInput (subTreeStringParse input) (subFormulasStringParse (filter (/= ' ') input)) minInputTrees correctFormulas correctTrees

judgeInput :: Either ParseError (Set (SynTree Op Char)) -> Either ParseError (Set String) -> Integer -> Set String -> Set (SynTree Op Char) -> Bool
judgeInput (Right inputTreesSet) (Right inputFormulasSet) minInputTrees correctFormulas correctTrees = inputTreesSet `isSubsetOf` correctTrees && inputFormulasSet `isSubsetOf` correctFormulas && fromIntegral (size inputFormulasSet) >= minInputTrees
judgeInput _ _ _ _ _ = False
