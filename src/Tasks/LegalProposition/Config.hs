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

data LegalPropositionConfig =
    LegalPropositionConfig
    {
      formulaConfig :: SynTreeConfig
    , formulas :: Integer
    , illegals :: Integer
    } deriving Show

checkAdditionalConfig :: LegalPropositionConfig -> Maybe String
checkAdditionalConfig LegalPropositionConfig {..}
    | formulas < illegals
      = Just "the number of formulas can not less than illegal number"
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
