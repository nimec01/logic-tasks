{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}

module Quiz (genSynTreeInst) where

import Test.QuickCheck ( generate )
import Print ( transfer )
import Types ( genSynTree, SynTree, display )
import Config (SynTreeConfig(SynTreeConfig, maxnode, minnode ,maxdepth,electliteral,mustcontain), SynTreeInst (image, SynTreeInst,insSyntree,correct))
import Data.Maybe (fromJust)

roll ::SynTreeConfig -> IO(SynTree , String, String)
roll SynTreeConfig{maxnode=maxnode , minnode=minnode ,..} = do
 tree <- generate $ fromJust (genSynTree (minnode,maxnode) maxdepth electliteral  mustcontain)
 let
  imag = transfer tree
  corr = display tree
 return (tree,imag,corr)

genSynTreeInst :: SynTreeConfig -> IO SynTreeInst
genSynTreeInst SynTreeConfig{maxnode=maxnode , minnode=minnode ,..} = do
 (a,b,c) <-  roll SynTreeConfig{..}
 return $ SynTreeInst
   { insSyntree=a
   , image=b
   , correct =c
   }
