{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SynTree.Quiz (
    feedback,
    generateSynTreeInst,
    ) where

import Test.QuickCheck (Gen)
import Tasks.SynTree.Config (SynTreeConfig(..), SynTreeInst(..))

import Trees.Print (display, transferToPicture)
import Trees.Parsing (formulaParse)
import Trees.Generate (genSynTree)

generateSynTreeInst :: SynTreeConfig -> Gen SynTreeInst
generateSynTreeInst SynTreeConfig {..} = do
    tree <- genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations
    return $ SynTreeInst
      { instSyntree = tree
      , latexImage = transferToPicture tree
      , correct = display tree
      }

feedback :: SynTreeInst -> String -> Bool
feedback SynTreeInst {instSyntree} input =
  formulaParse input == Right instSyntree
