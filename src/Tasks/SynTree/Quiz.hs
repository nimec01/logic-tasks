{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SynTree.Quiz (
    feedback,
    genSynTreeInst,
    ) where

import Test.QuickCheck (generate)
import Tasks.SynTree.Config (SynTreeConfig(..), SynTreeInst(..))

import Print (display, transferToPicture)
import Parsing (formulaParse)
import Generate (genSynTree)

genSynTreeInst :: SynTreeConfig -> IO SynTreeInst
genSynTreeInst SynTreeConfig {..} = do
    tree <- generate (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations)
    return $ SynTreeInst
      { instSyntree = tree
      , latexImage = transferToPicture tree
      , correct = display tree
      }

feedback :: SynTreeInst -> String -> Bool
feedback SynTreeInst {instSyntree} input =
  formulaParse input == Right instSyntree
