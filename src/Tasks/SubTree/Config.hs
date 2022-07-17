{-# LANGUAGE RecordWildCards #-}

module Tasks.SubTree.Config (
    SubtreeInst(..),
    SubtreeConfig(..),
    defaultSubtreeConfig,
    checkSubTreeConfig
  ) where

import Data.Set (Set)
import Control.Applicative              (Alternative ((<|>)))

import Types (SynTree)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)
import Generate (maxNodesForDepth)

data SubtreeConfig =
  SubtreeConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , useDupelTree :: Bool
    , minSubtreeNum :: Integer
    } deriving Show

defaultSubtreeConfig :: SubtreeConfig
defaultSubtreeConfig =
    SubtreeConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , useDupelTree = True
    , minSubtreeNum = 5
    }

checkSubTreeConfig :: SubtreeConfig -> Maybe String
checkSubTreeConfig subConfig@SubtreeConfig {..} =
    checkSynTreeConfig syntaxTreeConfig
    <|> checkAdditionalConfig subConfig

checkAdditionalConfig :: SubtreeConfig -> Maybe String
checkAdditionalConfig SubtreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
    | maxNodes < minSubtreeNum
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
