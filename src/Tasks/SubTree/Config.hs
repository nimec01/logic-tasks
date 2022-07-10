{-# LANGUAGE RecordWildCards #-}

module Tasks.SubTree.Config (
    SubtreeConfig(..),
    dSubtreeConfig,
    SubtreeInst(..)
  ) where

import Types (SynTree)
import Data.Set (Set)

data SubtreeConfig =
  SubtreeConfig 
  {
    maxnode :: Integer
  , minnode :: Integer
  , maxdepth :: Integer
  , electliteral :: String
  , mustcontain :: Integer
  , addoper :: Bool
  , useDupTree :: Bool
  , subtreeNub :: Integer
  } deriving Show

dSubtreeConfig :: SubtreeConfig
dSubtreeConfig =
    SubtreeConfig
    { maxnode = 8
    , minnode = 4
    , maxdepth =4
    , electliteral ="ABCDE"
    , mustcontain = 3
    , addoper = True
    , useDupTree = True
    , subtreeNub = 5
    }

data SubtreeInst =
    SubtreeInst
    { insSynTree :: SynTree Char
    , formula :: String
    , correct :: Set (SynTree Char)
    } deriving Show