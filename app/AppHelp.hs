{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module AppHelp (
    offerChange,
    determineBaseConfig,
) where

import Tasks.SynTree.Config (SynTreeConfig(..))

offerChange :: (Show a, Read a) => String -> a -> IO a
offerChange name value = do
  putStrLn $ "\nIf you want to change the setting " ++ name ++ " = " ++ show value ++ ", enter a new value here (otherwise just hit return):"
  input <- getLine
  if null input
    then return value
    else return (read input)

determineBaseConfig :: SynTreeConfig -> IO SynTreeConfig
determineBaseConfig initSynTreeConfig = do
  let SynTreeConfig{..} = initSynTreeConfig
  minNodes' <- offerChange "minNodes" minNodes
  maxNodes' <- offerChange "maxNodes" maxNodes
  maxDepth' <- offerChange "maxDepth" maxDepth
  atLeastOccurring' <- offerChange "atLeastOccurring" atLeastOccurring
  maxConsecutiveNegations' <- offerChange "maxConsecutiveNegations" maxConsecutiveNegations
  let newConfig = initSynTreeConfig { minNodes = minNodes', maxNodes = maxNodes', maxDepth = maxDepth', atLeastOccurring = atLeastOccurring', maxConsecutiveNegations = maxConsecutiveNegations'}
  return newConfig
