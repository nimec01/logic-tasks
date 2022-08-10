{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Main (main) where

import Tasks.SynTree.Config (SynTreeInst(..), SynTreeConfig, defaultSynTreeConfig, checkSynTreeConfig)
import Tasks.SynTree.Quiz (genSynTreeInst, feedback)
import AppHelp (determineBaseConfig, feedbackLoop)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineSynTreeConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SynTreeInst{ correct } <- genSynTreeInst theConfigToUse
  pPrint inst
  putStrLn "\nYour task is to input the propositional logic formula represented by the above (LaTeX rendered) syntax tree."
  putStrLn "You may use as many brackets as you want for your own clarity."
  feedbackLoop (feedback inst) ("The sample solution is: " ++ correct)

determineSynTreeConfig :: IO SynTreeConfig
determineSynTreeConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultSynTreeConfig
  syntaxTreeConfig <- determineBaseConfig defaultSynTreeConfig
  case checkSynTreeConfig syntaxTreeConfig of
    Nothing ->
      return syntaxTreeConfig
    Just problem -> do
      putStrLn $ "This didn't go well. Here is the problem: " ++ problem
      putStrLn "You should try again."
      determineSynTreeConfig
