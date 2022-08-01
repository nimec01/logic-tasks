{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Main (main) where

import Tasks.SubTree.Config (SubTreeInst(..),)
import Tasks.SubTree.Quiz (genSubTreeInst, feedback)
import AppHelp (determineSubTreeConfig)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineSubTreeConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SubTreeInst{..} <- genSubTreeInst theConfigToUse
  pPrint inst
  putStrLn ("\nThe task will give a formula and your task is to input a set which at least have " ++ show minInputTrees ++ " non-atomic Formulas")
  putStrLn "\nInput form is {subformula1,subformula2..}"
  putStrLn "\nDo not keep unnecessary parentheses outside subformulas and do not add any addtional parentheses"
  let
    feedbackLoop = do
      putStrLn "\nTry what feedback you will get for some input (blank for the sample solution):"
      input <- getLine
      if null input
        then
          putStrLn $ "One of the sample solution is " ++ show correctFormulas
        else do
          putStrLn $ "Your submission is " ++ show (feedback inst input)
          feedbackLoop
  feedbackLoop
