{-# LANGUAGE RecordWildCards #-}

module Tasks.SynTree.Config (
  SynTreeConfig(..),
  SynTreeInst(..),

  checkSynTreeConfig,
  defaultSynTreeConfig,
  ) where

import Types (SynTree, Op)
import Generate (maxNodesForDepth)
import Data.Char (isLetter)

data SynTreeConfig =
  SynTreeConfig
  { minNodes :: Integer
  , maxNodes :: Integer
  , maxDepth :: Integer
  , usedLiterals :: String
  , atLeastOccurring :: Integer
  , useImplEqui :: Bool
  , maxConsecutiveNegations :: Integer
  } deriving Show

checkSynTreeConfig :: SynTreeConfig -> Maybe String
checkSynTreeConfig SynTreeConfig {..}
    | any (not . isLetter) usedLiterals
      = Just "Only letters are allowed as literals."
    | maxConsecutiveNegations < 0
      = Just "Minimal number of consecutive negations must not be negative"
    | maxConsecutiveNegations == 0 && (even maxNodes || even minNodes)
      = Just "Syntax tree with no negation can not have even nodes"
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
    | let maxNodes' = maxNodes - 1
          maxConsecutiveNegations' = maxConsecutiveNegations + 2
          (result, rest) = maxNodes' `divMod` maxConsecutiveNegations', maxDepth > 1 + result * (maxConsecutiveNegations + 1) + min maxConsecutiveNegations rest
      = Just "A tree cannot be deeper than the maximum depth you set."
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
    , maxConsecutiveNegations = 2
    }

data SynTreeInst =
    SynTreeInst
    { instSyntree :: SynTree Op Char
    , latexImage :: String
    , correct :: String
    } deriving Show
