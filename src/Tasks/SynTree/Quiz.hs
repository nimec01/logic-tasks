{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SynTree.Quiz (
  genSynTreeInst,
  feedback,
  ) where

import Test.QuickCheck (generate)
import Parsing (normParse)
import Generate (genSynTree)
import Tasks.SynTree.Config (SynTreeConfig(..), SynTreeInst(..))
import Print ( transfer, display )

genSynTreeInst :: SynTreeConfig -> IO SynTreeInst
genSynTreeInst SynTreeConfig{..} = do
 tree <- generate (genSynTree (minnode,maxnode) maxdepth electliteral mustcontain addoper)
 return $ SynTreeInst
   { insSyntree = tree
   , image = transfer tree
   , correct = display tree
   }

feedback :: SynTreeInst -> String -> Bool
feedback SynTreeInst{insSyntree} input =
  normParse input == Right insSyntree
