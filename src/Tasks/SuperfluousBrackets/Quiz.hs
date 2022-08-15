{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SuperfluousBrackets.Quiz (
    feedback,
    generateSuperfluousBracketsInst
)where

import Tasks.SuperfluousBrackets.Config (SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..), )
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets(superfluousBracketsDisplay)
import Test.QuickCheck (Gen, suchThat)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Print (simplestDisplay)
import Trees.Helpers (sameAssociativeOperatorAdjacent)
import Trees.Generate (genSynTree)
import Tasks.SuperfluousBrackets.Parsing (superfluousBracketsExcParser)
import Data.Char (isSpace)

generateSuperfluousBracketsInst :: SuperfluousBracketsConfig -> Gen SuperfluousBracketsInst
generateSuperfluousBracketsInst SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    syntaxTree <- genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations
      `suchThat` sameAssociativeOperatorAdjacent
    stringWithSuperfluousBrackets <- superfluousBracketsDisplay syntaxTree superfluousBracketPairs
    return $ SuperfluousBracketsInst
      { stringWithSuperfluousBrackets
      , simplestString = simplestDisplay syntaxTree
      }

feedback :: SuperfluousBracketsInst -> String -> Bool
feedback SuperfluousBracketsInst {simplestString} input = superfluousBracketsExcParser input == Right (filter (not . isSpace) simplestString)
