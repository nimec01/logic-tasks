{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    genSubtreeInst
) where
import Test.QuickCheck (generate)
import Generate (genSynTreeSubtreeExc)
import Tasks.SubTree.Config (SubtreeConfig(..), SubtreeInst(..))
import Print (display)
import Types (allSubtre)

genSubtreeInst :: SubtreeConfig -> IO SubtreeInst
genSubtreeInst SubtreeConfig{..} = do
 tree <- generate (genSynTreeSubtreeExc (minnode,maxnode) maxdepth electliteral mustcontain addoper useDupTree subtreeNub)
 return $ SubtreeInst
   { insSynTree = tree
   , formula = display tree
   , correct = allSubtre tree
   }