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
    | illegals < 1
      = Just "the number of illegals can not less than 1"
    | formulas < illegals
      = Just "the number of formulas can not less than illegal number"
    | let leaves = maxLeavesForNodes maxNodes , max 1 ((maxNodes - leaves) ^ if useImplEqui then (4 :: Integer) else (2 :: Integer)) * (leaves * fromIntegral (length usedLiterals)) < formulas
      = Just "It have risks that formulas are larger than is actually reasonable given the possible size of the original formula"
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
