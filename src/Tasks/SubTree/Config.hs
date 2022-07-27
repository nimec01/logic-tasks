{-# LANGUAGE RecordWildCards #-}

module Tasks.SubTree.Config (
    SubTreeInst(..),
    SubTreeConfig(..),
    defaultSubTreeConfig,
    checkSubTreeConfig
  ) where

import Data.Set (Set)
import Control.Applicative              (Alternative ((<|>)))

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
    | minSubTrees < 1
      = Just "The task makes no sense if not at least one subtree should be given."
    | minNodes - maxLeavesForNodes minNodes < minSubTrees
      = Just "In this case, it is possible to have too much leaves nodes and lead to not enough non-atomic SubTrees"
    | otherwise = Nothing

data SubTreeInst =
    SubTreeInst
    { formula :: String
    , correctFormulas :: Set String
    , minInputTrees :: Integer
    } deriving Show
