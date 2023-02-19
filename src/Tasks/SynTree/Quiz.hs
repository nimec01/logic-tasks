{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SynTree.Quiz (
    feedback,
    generateSynTreeInst,
    ) where


import Test.QuickCheck (Gen)

import Tasks.SynTree.Config (SynTreeConfig(..), SynTreeInst(..))
import Trees.Generate (genSynTree)
import Trees.Print (display, transferToPicture)
import Trees.Types (BinOp, SynTree)




generateSynTreeInst :: SynTreeConfig -> Gen SynTreeInst
generateSynTreeInst SynTreeConfig {..} = do
    tree <- genSynTree
      (minNodes, maxNodes)
      maxDepth
      usedLiterals
      atLeastOccurring
      allowArrowOperators
      maxConsecutiveNegations
    return $ SynTreeInst
      { tree
      , latexImage = transferToPicture tree
      , correct = display tree
      }



feedback :: SynTreeInst -> SynTree BinOp Char -> Bool
feedback SynTreeInst {tree} input =
  input == tree
