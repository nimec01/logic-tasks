{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Main (main) where

import Tasks.SynTree.Config (SynTreeInst(..))
import Tasks.SynTree.Quiz (genSynTreeInst, feedback)
import AppHelp (determineSynTreeConfig)

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
