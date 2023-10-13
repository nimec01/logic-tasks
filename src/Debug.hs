{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Debug where

import Test.QuickCheck
import Control.Monad.Output.Generic
import Control.Monad.Output
import Text.Parsec
import Text.Parsec.String (Parser)

showDescription :: (m ~ (GenericReportT Language (IO ()) IO)) => Gen inst -> (inst -> LangM m) -> IO (Maybe ())
showDescription gen f = do
  inst <- generate gen
  runLangMReport (return ()) (>>) (f inst) >>= \(r, x) -> (x German :: IO ()) >> return r :: IO (Maybe ())

testTask ::
  (m ~ (GenericReportT Language (IO ()) IO), Show a) =>
  Gen inst ->
  (inst -> LangM m) ->
  (inst -> a -> LangM m) ->
  (inst -> a -> LangM m) ->
  Parser a ->
  IO ()
testTask gen f partial full p = do
  inst <- generate gen
  desc <- runLangMReport (return ()) (>>) (f inst) >>= \(r, x) -> (x German :: IO ()) >> return r :: IO (Maybe ())
  print desc
  str <- getLine
  case parse p "input" str of
    Left err -> print err
    Right value -> do
      putStrLn "---- Gelesen ----"
      print value
      putStrLn "---- Partial ----"
      partialRes <- runLangMReport (return ()) (>>) (partial inst value) >>= \(r, x) -> (x German :: IO ()) >> return r :: IO (Maybe ())
      print partialRes
      putStrLn "---- Complete ----"
      completeRes <- runLangMReport (return ()) (>>) (full inst value) >>= \(r, x) -> (x German :: IO ()) >> return r :: IO (Maybe ())
      print completeRes
