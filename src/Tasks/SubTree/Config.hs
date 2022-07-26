{-# LANGUAGE RecordWildCards #-}

module Tasks.SubTree.Config (
    SubTreeInst(..),
    SubTreeConfig(..),
    defaultSubTreeConfig,
    checkSubTreeConfig
  ) where

import Data.Set (Set)
import Control.Applicative              (Alternative ((<|>)))

import Types (SynTree)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)
import Generate (maxLeavesForNodes)

data SubTreeConfig =
  SubTreeConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , allowDupelTree :: Bool
    , minSubTrees :: Integer
    } deriving Show

defaultSubTreeConfig :: SubTreeConfig
defaultSubTreeConfig =
    SubTreeConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , allowDupelTree = True
    , minSubTrees = 3
    }

checkSubTreeConfig :: SubTreeConfig -> Maybe String
checkSubTreeConfig subConfig@SubTreeConfig {..} =
    checkSynTreeConfig syntaxTreeConfig
    <|> checkAdditionalConfig subConfig

checkAdditionalConfig :: SubTreeConfig -> Maybe String
checkAdditionalConfig SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
    | minNodes - maxLeavesForNodes minNodes < minSubTrees
      = Just "any trees have x min. nodes at least have x - (x+1) \\ 2 internal nodes, min. non-atomic SubTrees should not larger than it"
    | otherwise = Nothing

data SubTreeInst =
    SubTreeInst
    { formula :: String
    , correct :: Set (SynTree Char)
    , minInputTrees :: Integer
    } deriving Show
