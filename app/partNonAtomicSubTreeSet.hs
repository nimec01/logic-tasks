{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Tasks.SubTree.Config (defaultSubTreeConfig, SubTreeInst(..))
import Tasks.SubTree.Quiz (genSubTreeInst, feedback)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultSubTreeConfig
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SubTreeInst{ correctFormulas, minInputTrees} <- genSubTreeInst defaultSubTreeConfig
  putStrLn ("\nThe task will give a formula and you should input at least " ++ show minInputTrees ++ " non-atomic Formulas")
  putStrLn "\nInput in the form \"{subformula1,subformula2..}\" and do not keep unnecessary parentheses outside subformulas"
  pPrint inst
  let
    feedbackLoop = do
      putStrLn "\nTry what feedback you will get for some input (blank for the sample solution):"
      input <- getLine
      if null input
        then
          putStrLn $ "The sample solution is " ++ show correctFormulas
        else do
          putStrLn $ "Your submission is " ++ show (feedback inst input)
          feedbackLoop
  feedbackLoop
