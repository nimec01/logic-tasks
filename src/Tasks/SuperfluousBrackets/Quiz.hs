{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SuperfluousBrackets.Quiz (
    feedback,
    genSuperfluousBracketsInst,
    generateSuperfluousBracketsInst
)where

import Tasks.SuperfluousBrackets.Config (SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..), )
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets(superfluousBracketsDisplay)
import Test.QuickCheck (generate, Gen)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Print (simplestDisplay)
import Generate ( genSynTreeSuperfluousBracketsExc )
import Parsing(superfluousBracketsExcParser)

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

feedback :: SuperfluousBracketsInst -> String -> Bool
feedback SuperfluousBracketsInst {simplestString} input = superfluousBracketsExcParser input == Right simplestString
