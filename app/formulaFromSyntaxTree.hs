{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Config (dSynTreeConfig, SynTreeInst(..))
import Quiz (genSynTreeInst)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)
import Parsing (normParse)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "\nThe following is the default config:\n"
  pPrint dSynTreeConfig
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SynTreeInst{ correct } <- genSynTreeInst dSynTreeConfig
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

feedback :: SynTreeInst -> String -> Bool
feedback syntreeInst input = Right (insSyntree syntreeInst) ==  normParse input
