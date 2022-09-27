{-# LANGUAGE RecordWildCards #-}

module Tasks.LegalProposition.Config (
    LegalPropositionConfig (..),
    LegalPropositionInst (..),

    defaultLegalPropositionConfig,
    checkLegalPropositionConfig
) where

import Control.Applicative              (Alternative ((<|>)))
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)
import Data.Set (Set)
import Trees.Helpers (maxLeavesForNodes)

data LegalPropositionConfig =
    LegalPropositionConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , formulas :: Integer
    , illegals :: Integer
    , bracketFormulas :: Integer
    } deriving Show

defaultLegalPropositionConfig :: LegalPropositionConfig
defaultLegalPropositionConfig =
    LegalPropositionConfig
    {
      syntaxTreeConfig = defaultSynTreeConfig
    , formulas = 5
    , illegals = 2
    , bracketFormulas = 1
    }

checkLegalPropositionConfig :: LegalPropositionConfig -> Maybe String
checkLegalPropositionConfig lPConfig@LegalPropositionConfig {..} =
    checkSynTreeConfig syntaxTreeConfig
    <|> checkAdditionalConfig lPConfig

checkAdditionalConfig :: LegalPropositionConfig -> Maybe String
checkAdditionalConfig LegalPropositionConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
    | minNodes < 3
      = Just "form A and ~A is meaningless in this kind of issue"
    | formulas < 1
      = Just "The number of formulas must be positive."
    | illegals < 0
      = Just "The number of illegals can not be negative."
    | bracketFormulas < 0
      = Just "The number of bracketFormulas cannot be less than 0."
    | formulas < illegals + bracketFormulas
      = Just "The number of formulas cannot be less than the sum of bracket Formulas and illegal ones."
    | let leaves = maxLeavesForNodes maxNodes , (if allowArrowOperators then (4 :: Integer) else (2 :: Integer)) ^ (maxNodes - leaves) < formulas
      = Just "It has risks that formulas are larger than is actually reasonable given the possible size of the original formula."
    | otherwise
      = Nothing

data LegalPropositionInst =
    LegalPropositionInst
    {
      serialsOfWrong :: Set Int
    , pseudoFormulas :: [String]
    } deriving Show
