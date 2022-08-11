{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SuperfluousBrackets.Quiz (
    feedback,
    genSuperfluousBracketsInst,
    generateSuperfluousBracketsInst,
    genSynTreeSuperfluousBracketsExc
)where

import Tasks.SuperfluousBrackets.Config (SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..), )
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets(superfluousBracketsDisplay)
import Test.QuickCheck (generate, Gen, suchThat)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Print (simplestDisplay)
import Trees.Types (SynTree, Op)
import Trees.Helpers (sameAssociativeOperatorAdjacent)
import Trees.Generate (genSynTree)
import Tasks.SuperfluousBrackets.Parsing (superfluousBracketsExcParser)

genSuperfluousBracketsInst :: SuperfluousBracketsConfig -> IO SuperfluousBracketsInst
genSuperfluousBracketsInst sBConfig =generate (generateSuperfluousBracketsInst sBConfig)

generateSuperfluousBracketsInst :: SuperfluousBracketsConfig -> Gen SuperfluousBracketsInst
generateSuperfluousBracketsInst SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    syntaxTree <- genSynTreeSuperfluousBracketsExc (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations
    superfluousString <- superfluousBracketsDisplay syntaxTree superfluousBrackets
    return $ SuperfluousBracketsInst
      { superfluousString
      , simplestString = simplestDisplay syntaxTree
      }

genSynTreeSuperfluousBracketsExc :: (Integer, Integer) -> Integer -> String -> Integer -> Bool -> Integer -> Gen (SynTree Op Char)  --minNodes must >= 5
genSynTreeSuperfluousBracketsExc (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui maxConsecutiveNegations =
    genSynTree (minNodes, maxNodes) maxDepth availableLetters atLeastOccurring useImplEqui maxConsecutiveNegations `suchThat` sameAssociativeOperatorAdjacent

feedback :: SuperfluousBracketsInst -> String -> Bool
feedback SuperfluousBracketsInst {simplestString} input = superfluousBracketsExcParser input == Right simplestString
