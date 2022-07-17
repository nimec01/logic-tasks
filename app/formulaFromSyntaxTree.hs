{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Tasks.SynTree.Config (defaultSynTreeConfig, SynTreeInst(..))
import Tasks.SynTree.Quiz (genSynTreeInst, feedback)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultSynTreeConfig
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SynTreeInst{ correct } <- genSynTreeInst defaultSynTreeConfig
  pPrint inst
  let
    feedbackLoop = do
      putStrLn "\nTry what feedback you will get for some input (blank for the sample solution):"
      input <- getLine
      if null input
        then
          putStrLn $ "The sample solution is " ++ show (feedback inst correct)
        else do
          putStrLn $ "Your submission is " ++ show (feedback inst input)
          feedbackLoop
  feedbackLoop
