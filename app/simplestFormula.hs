{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Main (main) where

import Tasks.SuperfluousBrackets.Config (defaultSuperfluousBracketsConfig, SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..), checkSuperfluousBracketsConfig)
import Tasks.SuperfluousBrackets.Quiz (genSuperfluousBracketsInst, feedback)
import AppHelp (offerChange, determineBaseConfig, feedbackLoop)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineSuperfluousBracketsConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SuperfluousBracketsInst{ simplestString } <- genSuperfluousBracketsInst theConfigToUse
  pPrint inst
  putStrLn "\n This is a important syntax task before you deal with CNF and DNF"
  putStrLn "\n Because of /\\ and \\/ are associative, it is not necessary to use brackets when combining three atoms with same operators /\\ or \\/ for example A/\\B/\\C"
  putStrLn "\n The task will give a formula with redundant brackets, and your mission is to give the simplest form of the formula"
  putStrLn "\n That means delete all unnecessary brackets"
  feedbackLoop (feedback inst) ("The sample solution is: " ++ simplestString)

determineSuperfluousBracketsConfig :: IO SuperfluousBracketsConfig
determineSuperfluousBracketsConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultSuperfluousBracketsConfig
  let SuperfluousBracketsConfig{..} = defaultSuperfluousBracketsConfig
  syntaxTreeConfig' <- determineBaseConfig syntaxTreeConfig
  superfluousBracketPairs' <- offerChange "superfluousBracketPairs" superfluousBracketPairs
  let newConfig = defaultSuperfluousBracketsConfig {syntaxTreeConfig = syntaxTreeConfig', superfluousBracketPairs = superfluousBracketPairs'}
  case checkSuperfluousBracketsConfig newConfig of
    Nothing ->
      return newConfig
    Just problem -> do
      putStrLn $ "This didn't go well. Here is the problem: " ++ problem
      putStrLn "You should try again."
      determineSuperfluousBracketsConfig
