{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module AppHelp (
    determineSynTreeConfig,
    determineLegalPropositionConfig,
    determineSubTreeConfig
)where

import Tasks.SynTree.Config(SynTreeConfig(..), defaultSynTreeConfig,checkSynTreeConfig)
import Tasks.LegalProposition.Config (defaultLegalPropositionConfig, LegalPropositionConfig(..), checkLegalPropositionConfig)
import Tasks.SubTree.Config (defaultSubTreeConfig, SubTreeConfig(..), checkSubTreeConfig)

import Text.Pretty.Simple (pPrint)

offerChange :: (Show a, Read a) => String -> a -> IO a
offerChange name value = do
  putStrLn $ "\nIf you want to change the setting " ++ name ++ " = " ++ show value ++ ", enter a new value here (otherwise just hit return):"
  input <- getLine
  if null input
    then return value
    else return (read input)

determineConfig :: SynTreeConfig -> IO SynTreeConfig
determineConfig initSynTreeConfig = do
  let SynTreeConfig{..} = initSynTreeConfig
  minNodes' <- offerChange "minNodes" minNodes
  maxNodes' <- offerChange "maxNodes" maxNodes
  maxDepth' <- offerChange "maxDepth" maxDepth
  atLeastOccurring' <- offerChange "atLeastOccurring" atLeastOccurring
  let newConfig = initSynTreeConfig { minNodes = minNodes', maxNodes = maxNodes', maxDepth = maxDepth', atLeastOccurring = atLeastOccurring' }
  return newConfig

determineSynTreeConfig :: IO SynTreeConfig
determineSynTreeConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultSynTreeConfig
  syntaxTreeConfig <- determineConfig defaultSynTreeConfig
  case checkSynTreeConfig syntaxTreeConfig of
    Nothing ->
      return syntaxTreeConfig
    Just problem -> do
      putStrLn $ "This didn't go well. Here is the problem: " ++ problem
      putStrLn "You should try again."
      determineSynTreeConfig

determineLegalPropositionConfig :: IO LegalPropositionConfig
determineLegalPropositionConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultLegalPropositionConfig
  let LegalPropositionConfig {..} = defaultLegalPropositionConfig
  syntaxTreeConfig' <- determineConfig syntaxTreeConfig
  formulas' <- offerChange "formulas" formulas
  illegals' <- offerChange "illegals" illegals
  let newConfig = defaultLegalPropositionConfig {syntaxTreeConfig = syntaxTreeConfig', formulas = formulas', illegals = illegals'}
  case checkLegalPropositionConfig newConfig of
    Nothing ->
      return newConfig
    Just problem -> do
      putStrLn $ "This didn't go well. Here is the problem: " ++ problem
      putStrLn "You should try again."
      determineLegalPropositionConfig

determineSubTreeConfig :: IO SubTreeConfig
determineSubTreeConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultSubTreeConfig
  let SubTreeConfig{..} = defaultSubTreeConfig
  syntaxTreeConfig' <- determineConfig syntaxTreeConfig
  allowDupelTree' <- offerChange "allowDupelTree" allowDupelTree
  minSubTrees' <- offerChange "minSubTrees" minSubTrees
  let newConfig = defaultSubTreeConfig {syntaxTreeConfig = syntaxTreeConfig', minSubTrees = minSubTrees', allowDupelTree = allowDupelTree'}
  case checkSubTreeConfig newConfig of
    Nothing ->
      return newConfig
    Just problem -> do
      putStrLn $ "This didn't go well. Here is the problem: " ++ problem
      putStrLn "You should try again."
      determineSubTreeConfig
