{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    genSubtreeInst
) where
import Test.QuickCheck (generate)
import Generate (genSynTreeSubtreeExc)

import Tasks.SubTree.Config (SubtreeConfig(..), SubtreeInst(..))
import Print (display)
import Types (allSubtree)

genSubtreeInst :: SubtreeConfig -> IO SubtreeInst
genSubtreeInst SubtreeConfig {..} = do
    tree <- generate (genSynTreeSubtreeExc (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui useDupelTree subtreeNum)
    return $ SubtreeInst
      { insSynTree = tree
      , formula = display tree
      , correct = allSubtree tree
      }