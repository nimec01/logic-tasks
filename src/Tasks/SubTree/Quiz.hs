{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    feedback,
    genSubTreeInst,
) where

import Test.QuickCheck (generate)
import Generate (genSynTreeSubTreeExc)
import Data.Set (Set, isSubsetOf, size, map)

import Parsing (subFormulasStringParse)
import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), SubTreeInst)
import Print (display)
import Types (allNotLeafSubTrees)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Text.Parsec (ParseError)


genSubTreeInst :: SubTreeConfig -> IO SubTreeInst
genSubTreeInst SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- generate (genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui allowDupelTree minSubTrees)
    return $ SubTreeInst
      { minInputTrees = minSubTrees
      , formula = display tree
      , correctFormulas = Data.Set.map display (allNotLeafSubTrees tree)
      }

feedback :: SubTreeInst -> String -> Bool
feedback SubTreeInst {correctFormulas, minInputTrees} input = judgeInput (subFormulasStringParse (filter (/= ' ') input)) minInputTrees correctFormulas

judgeInput :: Either ParseError (Set String) -> Integer -> Set String -> Bool
judgeInput (Right inputFormulaSet) minInputTrees correctFormulas = inputFormulaSet `isSubsetOf` correctFormulas && fromIntegral (size inputFormulaSet) >= minInputTrees
judgeInput _ _ _ = False
