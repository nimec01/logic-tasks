{-# LANGUAGE RecordWildCards #-}

module Tasks.SuperfluousBrackets.Config(
    SuperfluousBracketsConfig (..),
    SuperfluousBracketsInst (..),

    defaultSuperfluousBracketsConfig,
    checkSuperfluousBracketsConfig
)where

import Control.Applicative              (Alternative ((<|>)))

import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)

data SuperfluousBracketsConfig =
    SuperfluousBracketsConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , superfluousBracketPairs :: Integer
    } deriving Show

defaultSuperfluousBracketsConfig :: SuperfluousBracketsConfig
defaultSuperfluousBracketsConfig =
    SuperfluousBracketsConfig
    {
      syntaxTreeConfig = defaultSynTreeConfig { useImplEqui = True }
    , superfluousBracketPairs = 2
    }

checkSuperfluousBracketsConfig :: SuperfluousBracketsConfig -> Maybe String
checkSuperfluousBracketsConfig sBConfig@SuperfluousBracketsConfig {..} =
    checkSynTreeConfig syntaxTreeConfig
    <|> checkAdditionalConfig sBConfig

checkAdditionalConfig :: SuperfluousBracketsConfig -> Maybe String
checkAdditionalConfig SuperfluousBracketsConfig {syntaxTreeConfig=SynTreeConfig {..}, ..}
    | minNodes < 5
      = Just "Minimal number of nodes must larger than 4"
    | superfluousBracketPairs >= minNodes
      = Just "The number of Superfluous brackets is too much, it should not larger than minimal number of nodes"
    | superfluousBracketPairs < 1
      = Just "Add at least one extra Brackets"
    | otherwise
      = Nothing

data SuperfluousBracketsInst =
    SuperfluousBracketsInst
    {
      stringWithSuperfluousBrackets :: String
    , simplestString :: String
    } deriving Show
