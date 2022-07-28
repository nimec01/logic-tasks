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
import Generate(maxLeavesForNodes)

data LegalPropositionConfig =
    LegalPropositionConfig
    {
      formulaConfig :: SynTreeConfig
    , formulas :: Integer
    , illegals :: Integer
    } deriving Show

checkAdditionalConfig :: LegalPropositionConfig -> Maybe String
checkAdditionalConfig LegalPropositionConfig {formulaConfig = SynTreeConfig {..}, ..}
    | formulas < 1
      = Just "The number of formulas cannot be less than 1."
    | illegals < 0
      = Just "The number of illegals cannot be less than 0."
    | formulas < illegals
      = Just "The number of formulas cannot be less than the illegal ones."
    | let leaves = maxLeavesForNodes maxNodes , max 1 ((maxNodes - leaves) ^ if useImplEqui then (4 :: Integer) else (2 :: Integer)) * (leaves * fromIntegral (length usedLiterals)) < formulas
      = Just "It has risks that formulas are larger than is actually reasonable given the possible size of the original formula."
    | otherwise
      = Nothing

checkLegalPropositionConfig :: LegalPropositionConfig -> Maybe String
checkLegalPropositionConfig lPConfig@LegalPropositionConfig {..} =
    checkSynTreeConfig formulaConfig
    <|> checkAdditionalConfig lPConfig

defaultLegalPropositionConfig :: LegalPropositionConfig
defaultLegalPropositionConfig =
    LegalPropositionConfig
    {
      formulaConfig = defaultSynTreeConfig
    , formulas = 5
    , illegals = 2
    }

data LegalPropositionInst =
    LegalPropositionInst
    {
      serialsOfWrong :: Set Int
    , pseudoFormulas :: [String]
    } deriving Show
