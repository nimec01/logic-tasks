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
    | formulas < 1
      = Just "The number of formulas cannot be less than 1."
    | illegals < 0
      = Just "The number of illegals cannot be less than 0."
    | bracketFormulas < 0
      = Just "The number of bracketFormulas cannot be less than 0."
    | formulas < illegals + bracketFormulas
      = Just "The number of formulas cannot be less than the sum of bracket Formulas and illegal ones."
    | let leaves = maxLeavesForNodes maxNodes , max 1 ((maxNodes - leaves) ^ if useImplEqui then (4 :: Integer) else (2 :: Integer)) < formulas
      = Just "It has risks that formulas are larger than is actually reasonable given the possible size of the original formula."
    | otherwise
      = Nothing

data LegalPropositionInst =
    LegalPropositionInst
    {
      serialsOfWrong :: Set Int
    , pseudoFormulas :: [String]
    } deriving Show
