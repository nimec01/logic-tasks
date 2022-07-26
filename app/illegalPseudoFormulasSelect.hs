{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Tasks.LegalProposition.Config (defaultLegalPropositionConfig, LegalPropositionInst(..))
import Tasks.LegalProposition.Quiz (genLegalPropositionInst, feedback)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultLegalPropositionConfig
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@LegalPropositionInst{ serialsOfWrong } <- genLegalPropositionInst defaultLegalPropositionConfig
  pPrint inst
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
