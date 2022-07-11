{-# LANGUAGE RecordWildCards #-}

module Tasks.SubTree.Config (
    SubtreeInst(..),
    SubtreeConfig(..),
    dSubtreeConfig,
  ) where

import Data.Set (Set)

import Types (SynTree)

data SubtreeConfig =
  SubtreeConfig
    {
      maxNode :: Integer
    , minNode :: Integer
    , maxDepth :: Integer
    , usedLiterals :: String
    , atLeastOccurring :: Integer
    , useImplEqui :: Bool
    , useDupelTree :: Bool
    , subtreeNum :: Integer
    } deriving Show

dSubtreeConfig :: SubtreeConfig
dSubtreeConfig =
    SubtreeConfig
    { maxNode = 8
    , minNode = 4
    , maxDepth = 4
    , usedLiterals = "ABCDE"
    , atLeastOccurring = 3
    , useImplEqui = True
    , useDupelTree = True
    , subtreeNum = 5
    }

data SubtreeInst =
    SubtreeInst
    { insSynTree :: SynTree Char
    , formula :: String
    , correct :: Set (SynTree Char)
    } deriving Show
