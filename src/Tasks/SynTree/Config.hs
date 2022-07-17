{-# LANGUAGE RecordWildCards #-}

module Tasks.SynTree.Config (
  SynTreeConfig(..),
  SynTreeInst(..),

  checkSynTreeConfig,
  defaultSynTreeConfig,
  ) where

import Types (SynTree)
import Generate (maxNodesForDepth)

data SynTreeConfig =
  SynTreeConfig
  { maxNodes :: Integer
  , minNodes :: Integer
  , maxDepth :: Integer
  , usedLiterals :: String
  , atLeastOccurring :: Integer
  , useImplEqui :: Bool
  } deriving Show

checkSynTreeConfig :: SynTreeConfig -> Maybe String
checkSynTreeConfig SynTreeConfig {..}
    | minNodes < 1
      = Just "Minimal number of nodes must be positive."
    | maxNodes < minNodes
      = Just "Maximal number of nodes must not be smaller than minimal number."
    | maxDepth < 1
      = Just "Non-positive depth makes no sense."
    | atLeastOccurring < 1
      = Just "At least one literal occurs in each formula."
    | fromIntegral (length usedLiterals) < atLeastOccurring
      = Just "You have provided too few literals."
    | minNodes < atLeastOccurring * 2 - 1
      = Just "Your minimum number of nodes does not permit enough leaves for all desired literals."
    | maxNodes > maxNodesForDepth maxDepth
      = Just "Your minimum number of nodes is larger than what your maximum depth enables."
    | maxDepth > maxNodes
      = Just "A tree cannot be deeper than its size."
    | otherwise = Nothing

defaultSynTreeConfig :: SynTreeConfig
defaultSynTreeConfig =
    SynTreeConfig
    { maxNodes = 10
    , minNodes = 6
    , maxDepth = 6
    , usedLiterals = "ABCDE"
    , atLeastOccurring = 3
    , useImplEqui = False
    }

data SynTreeInst =
    SynTreeInst
    { instSyntree :: SynTree Char
    , latexImage :: String
    , correct :: String
    } deriving Show
