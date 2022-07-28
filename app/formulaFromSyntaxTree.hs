{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Main (main) where

import Tasks.SynTree.Config (SynTreeConfig(..), defaultSynTreeConfig, checkSynTreeConfig, SynTreeInst(..))
import Tasks.SynTree.Quiz (genSynTreeInst, feedback)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SynTreeInst{ correct } <- genSynTreeInst theConfigToUse
  pPrint inst
  let
    feedbackLoop = do
      putStrLn "\nTry what feedback you will get for some input (blank for the sample solution and exit):"
      input <- getLine
      if null input
        then
          putStrLn $ "The sample solution is: " ++ correct
        else do
          putStrLn $ "Your submission is " ++ show (feedback inst input)
          feedbackLoop
  feedbackLoop

determineConfig :: IO SynTreeConfig
determineConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultSynTreeConfig
  let SynTreeConfig{..} = defaultSynTreeConfig
  minNodes <- offerChange "minNodes" minNodes
  maxNodes <- offerChange "maxNodes" maxNodes
  maxDepth <- offerChange "maxDepth" maxDepth
  atLeastOccurring <- offerChange "atLeastOccurring" atLeastOccurring
  let newConfig = defaultSynTreeConfig { minNodes, maxNodes, maxDepth, atLeastOccurring }
  case checkSynTreeConfig newConfig of
    Nothing ->
      return newConfig
    Just problem -> do
      putStrLn $ "This didn't go well. Here is the problem: " ++ problem
      putStrLn "You should try again."
      determineConfig

offerChange :: Read a => String -> a -> IO a
offerChange name value = do
  putStrLn $ "\nDo you want to change " ++ name ++ "? If so, enter new value here (otherwise just hit return):"
  input <- getLine
  if null input
    then return value
    else return (read input)
