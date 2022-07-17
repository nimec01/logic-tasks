{-# LANGUAGE RecordWildCards #-}

module Tasks.LegalProposition.Config (
    LegalPropositionConfig (..),
    LegalPropositionInst (..),

    defaultLegalPropositionConfig,
    checkLegalPropositionConfig
) where

import Control.Applicative              (Alternative ((<|>)))
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)

data LegalPropositionConfig =
    LegalPropositionConfig
    {
      formulaConfig :: SynTreeConfig
    , formulaNum :: Integer
    , illegalNum :: Integer
    } deriving Show

checkAdditionalConfig :: LegalPropositionConfig -> Maybe String
checkAdditionalConfig LegalPropositionConfig {..}
    | formulaNum < illegalNum
      = Just "the number of formula can not less than illegal number"
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
    , formulaNum = 5
    , illegalNum = 2
    }

data LegalPropositionInst =
    LegalPropositionInst
    {
      serialNumOfWrong :: [Int]
    , pseudoFormulas :: [String]
    } deriving Show
