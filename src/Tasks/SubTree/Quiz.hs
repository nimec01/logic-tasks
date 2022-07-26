{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    feedback,
    genSubTreeInst,
) where

import Test.QuickCheck (generate)
import Generate (genSynTreeSubTreeExc)
import Data.Set (Set, isSubsetOf, size)

import Parsing (subTreeStringParse)
import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), SubTreeInst)
import Print (display)
import Types (allNotLeafSubTrees, SynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Text.Parsec (ParseError)


genSubTreeInst :: SubTreeConfig -> IO SubTreeInst
genSubTreeInst SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- generate (genSynTreeSubTreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui allowDupelTree minSubTrees)
    return $ SubTreeInst
      { minInputTrees = minSubTrees
      , formula = display tree
      , correct = allNotLeafSubTrees tree
      }

feedback :: SubTreeInst -> String -> Bool
feedback SubTreeInst {correct, minInputTrees} input = judgeInput (subTreeStringParse input) minInputTrees correct

judgeInput :: Either ParseError (Set (SynTree Char)) -> Integer -> Set (SynTree Char) -> Bool
judgeInput (Right inputTreeSet) minInputTrees correctTreeSet = inputTreeSet `isSubsetOf` correctTreeSet && fromIntegral (size inputTreeSet) >= minInputTrees
judgeInput (Left _) _ _ = False
