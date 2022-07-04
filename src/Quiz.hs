{-# language MultiParamTypeClasses #-}
{-# language RecordWildCards #-}

module Quiz where

import Test.QuickCheck
import Data.Typeable
import GHC.Generics
import Print
import Types
import Config (SynTreeConfig(SynTreeConfig, maxnode, minnode ,maxdepth,electliteral,mustcontain), PickInst)
import Data.Maybe (fromJust)

--PickConfig -> IO ([Cnf], Int)
roll ::SynTreeConfig -> IO(SynTree , String, String)
roll SynTreeConfig{maxnode=maxnode , minnode=minnode ,..} = do
    tree <- generate $ fromJust (genSynTree (minnode,maxnode) maxdepth electliteral  mustcontain )
    let
        imag = transfer tree
        corr = show tree
    return (tree,imag,corr)

-- instance Generator Pick PickConfig PickInst where