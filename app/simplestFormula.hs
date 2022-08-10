{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Main (main) where

import Tasks.SuperfluousBrackets.Config (defaultSuperfluousBracketsConfig, SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..), checkSuperfluousBracketsConfig)
import Tasks.SuperfluousBrackets.Quiz (genSuperfluousBracketsInst, feedback)
import AppHelp (offerChange, determineBaseConfig)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineSuperfluousBracketsConfig
  putStrLn "\nThe following is the default config:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SuperfluousBracketsInst{ simplestString } <- genSuperfluousBracketsInst theConfigToUse
  pPrint inst
  putStrLn "\n The task will give a formula with redundant brackets, and your mission is to give the simplest form of the formula"
  putStrLn "\n That means delete all unnecessary brackets"
  let
    feedbackLoop = do
      putStrLn "\nTry what feedback you will get for some input (blank for the sample solution):"
      input <- getLine
      if null input
        then
          putStrLn $ "The sample solution is " ++ show (feedback inst simplestString)
        else do
          putStrLn $ "Your submission is " ++ show (feedback inst input)
          feedbackLoop
  feedbackLoop

determineSuperfluousBracketsConfig :: IO SuperfluousBracketsConfig
determineSuperfluousBracketsConfig = do
  putStrLn "\nThe following is the default config :\n"
  pPrint defaultSuperfluousBracketsConfig
  let SuperfluousBracketsConfig{..} = defaultSuperfluousBracketsConfig
  syntaxTreeConfig' <- determineBaseConfig syntaxTreeConfig
  superfluousBrackets' <- offerChange "superfluousBrackets" superfluousBrackets
  let newConfig = defaultSuperfluousBracketsConfig {syntaxTreeConfig = syntaxTreeConfig', superfluousBrackets = superfluousBrackets'}
  case checkSuperfluousBracketsConfig newConfig of
    Nothing ->
      return newConfig
    Just problem -> do
      putStrLn $ "This didn't go well. Here is the problem: " ++ problem
      putStrLn "You should try again."
      determineSuperfluousBracketsConfig
