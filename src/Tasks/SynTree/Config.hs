{-# LANGUAGE RecordWildCards #-}

module Tasks.SynTree.Config (
  SynTreeConfig(..),
  SynTreeInst(..),

  checkSynTreeConfig,
  dSynTreeConfig,
  ) where

import Types (SynTree)
import Generate (maxLeavesForNodes, maxNodesForDepth, rangeDepthForNodes,)

data SynTreeConfig =
  SynTreeConfig
  { maxNode :: Integer
  , minNode :: Integer
  , maxDepth :: Integer
  , usedLiterals :: String
  , atLeastOccurring :: Integer
  , useImplEqui :: Bool
  } deriving Show

checkSynTreeConfig :: SynTreeConfig -> Maybe String
checkSynTreeConfig SynTreeConfig {..}
    | minNode < 1
      = Just "Minimal number of nodes must be positive."
    | maxNode < minNode
      = Just "Maximal number of nodes must not be smaller than minimal number."
    | maxDepth < 1
      = Just "Non-positive depth makes no sense."
    | atLeastOccurring < 1
      = Just "At least one literal occurs in each formula."
    | fromIntegral (length usedLiterals) < atLeastOccurring
      = Just "You have provided too few literals."
    | fst (rangeDepthForNodes minNode) > maxDepth
      = Just "The number of nodes generated at the specific depth is less than the minimum number of nodes"
    | maxLeavesForNodes maxNode < atLeastOccurring
      = Just "The syntax tree generated by the maximum number of nodes cannot contain enough must contain Literals"
    | 2 ^ (maxDepth - 1) < atLeastOccurring
      = Just "The syntax tree with maximum depth cannot contain enough must contain Literals"
    | minNode < atLeastOccurring * 2 - 1
      = Just "Your minimum number of nodes does not permit enough leaves for all desired literals."
    | maxNode > maxNodesForDepth maxDepth
      = Just "Your minimum number of nodes is larger than what your maximum depth enables."
    | otherwise = Nothing

dSynTreeConfig :: SynTreeConfig
dSynTreeConfig =
    SynTreeConfig
    { maxNode = 10
    , minNode = 6
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