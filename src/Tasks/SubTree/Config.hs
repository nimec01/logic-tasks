{-# LANGUAGE RecordWildCards #-}

module Tasks.SubTree.Config (
    SubtreeInst(..),
    SubtreeConfig(..),
    dSubtreeConfig,
    checkSubTreeConfig
  ) where

import Data.Set (Set)
import Control.Applicative              (Alternative ((<|>)))

import Types (SynTree)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, dSynTreeConfig)
import Generate (maxNodesForDepth)

data SubtreeConfig =
  SubtreeConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , useDupelTree :: Bool
    , minSubtreeNum :: Integer
    } deriving Show

dSubtreeConfig :: SubtreeConfig
dSubtreeConfig =
    SubtreeConfig
    { syntaxTreeConfig = dSynTreeConfig
    , useDupelTree = True
    , minSubtreeNum = 5
    }

checkSubTreeConfig :: SynTreeConfig -> SubtreeConfig -> Maybe String
checkSubTreeConfig synConfig subConfig =
    checkSynTreeConfig synConfig
    <|> checkAdditionalConfig subConfig

checkAdditionalConfig :: SubtreeConfig -> Maybe String
checkAdditionalConfig SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
    | maxNode < minSubtreeNum
      = Just "The subtree's number can not larger than maxnode"
    | maxNodesForDepth maxDepth < minSubtreeNum
      = Just "The subtree's number can not larger than maxDepth can not contain"
    | otherwise = Nothing

data SubtreeInst =
    SubtreeInst
    { insSynTree :: SynTree Char
    , formula :: String
    , correct :: Set (SynTree Char)
    , minInputTreeNum :: Integer
    } deriving Show
