{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}

module Quiz where

import Test.QuickCheck ( generate )
import Print ( transfer )
import Types ( genSynTree, SynTree )
import Config (SynTreeConfig(SynTreeConfig, maxnode, minnode ,maxdepth,electliteral,mustcontain), PickInst (image, PickInst,insSyntree,correct))
import Data.Maybe (fromJust)

roll ::SynTreeConfig -> IO(SynTree , String, String)
roll SynTreeConfig{maxnode=maxnode , minnode=minnode ,..} = do
 tree <- generate $ fromJust (genSynTree (minnode,maxnode) maxdepth electliteral  mustcontain)
 let
  imag = transfer tree
  corr = show tree
 return (tree,imag,corr)

genPickInst :: SynTreeConfig -> IO PickInst
genPickInst SynTreeConfig{maxnode=maxnode , minnode=minnode ,..} = do
 (a,b,c) <-  roll SynTreeConfig{..}
 return $ PickInst { insSyntree=a
                     , image=b
                     , correct =c
                    }