{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Main (main) where

import Tasks.LegalProposition.Config (LegalPropositionInst(..))
import Tasks.LegalProposition.Quiz (genLegalPropositionInst, feedback)
import AppHelp (determineLegalPropositionConfig)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineLegalPropositionConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@LegalPropositionInst{..} <- genLegalPropositionInst theConfigToUse
  pPrint inst
  putStrLn "In this task there are some Pseudo Formulas, your task is to give the set of ordinal numbers of the illegal formulas in Pseudo Formulas"
  putStrLn "The input form is {serial number1, serial number2,..}"
  let
    feedbackLoop = do
      putStrLn "\nTry what feedback you will get for some input (blank for the sample solution):"
      input <- getLine
      if null input
        then
          putStrLn $ "The sample solution is " ++ show serialsOfWrong
        else do
          putStrLn $ "Your submission is " ++ show (feedback inst input)
          feedbackLoop
  feedbackLoop
