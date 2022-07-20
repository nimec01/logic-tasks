{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    feedback,
    genSubtreeInst,
) where

import Test.QuickCheck (generate)
import Generate (genSynTreeSubtreeExc)
import Data.Set (Set, isSubsetOf, size)

import Parsing (subtreeStringParse)
import Tasks.SubTree.Config (SubtreeConfig(..), SubtreeInst(..), SubtreeInst)
import Print (display)
import Types (allSubtrees, SynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Text.Parsec (ParseError)


genSubtreeInst :: SubtreeConfig -> IO SubtreeInst
genSubtreeInst SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- generate (genSynTreeSubtreeExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree minSubtreeNum)
    return $ SubtreeInst
      { insSynTree = tree
      , minInputTreeNum = minSubtreeNum
      , formula = display tree
      , correct = allSubtrees tree
      }

feedback :: SubtreeInst -> String -> Bool
feedback SubtreeInst {correct, minInputTreeNum} input = judgeInput (subtreeStringParse input) (fromIntegral minInputTreeNum) correct

judgeInput :: Either ParseError (Set (SynTree Char)) -> Int -> Set (SynTree Char) -> Bool
judgeInput (Right inputTreeSet) minInputTreeNum correctTreeSet = inputTreeSet `isSubsetOf` correctTreeSet && size inputTreeSet >= minInputTreeNum
judgeInput (Left _) _ _ =False
