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
import Trees.Types (SynTree, BinOp)
import Trees.Helpers (maxLeavesForNodes)

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
    | minSubTrees < 2
      = Just "The task makes no sense if not at least two subtrees should be given."
    | minNodes - maxLeavesForNodes minNodes < minSubTrees --because minSubTrees >= 2  then minnodes >=4
      = Just "In this case, it is possible to have too much leaves nodes and lead to not enough non-atomic SubTrees"
    | otherwise = Nothing

data SubTreeInst =
    SubTreeInst
    { formula :: String
    , correctTrees :: Set (SynTree BinOp Char)
    , correctFormulas :: Set String
    , minInputTrees :: Integer
    } deriving Show
