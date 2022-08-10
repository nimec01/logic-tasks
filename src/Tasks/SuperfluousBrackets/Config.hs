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
    , superfluousBrackets :: Integer
    } deriving Show

checkAdditionalConfig :: SuperfluousBracketsConfig -> Maybe String
checkAdditionalConfig SuperfluousBracketsConfig {syntaxTreeConfig=SynTreeConfig {..}, ..}
    | minNodes < 5
      = Just "Minimal number of nodes must larger than 4"
    | superfluousBrackets > fromIntegral minNodes
      = Just  "It is possible that occured ((formula)) and these two brackets are not necessary"
    | superfluousBrackets < 1
      = Just "Add at least one extra Brackets"
    | otherwise
      = Nothing

checkSuperfluousBracketsConfig :: SuperfluousBracketsConfig -> Maybe String
checkSuperfluousBracketsConfig sBConfig@SuperfluousBracketsConfig {..} =
    checkSynTreeConfig syntaxTreeConfig
    <|> checkAdditionalConfig sBConfig

defaultSuperfluousBracketsConfig :: SuperfluousBracketsConfig
defaultSuperfluousBracketsConfig =
    SuperfluousBracketsConfig
    {
      syntaxTreeConfig = defaultSynTreeConfig
    , superfluousBrackets = 2
    }

data SuperfluousBracketsInst =
    SuperfluousBracketsInst
    {
      superfluousString :: String
    , simplestString :: String
    } deriving Show
