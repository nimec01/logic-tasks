{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module LogicTasks.Debug where

import Test.QuickCheck
import Control.Monad.Output.Generic
import Control.Monad.Output
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (isJust)

showDescription :: (m ~ GenericReportT Language (IO ()) IO) => Gen inst -> (inst -> LangM m) -> IO (Maybe ())
showDescription gen f = do
  inst <- generate gen
  run (f inst)

testTask ::
  (m ~ GenericReportT Language (IO ()) IO, Show a) =>
  Gen inst ->
  (inst -> LangM m) ->
  (inst -> a -> LangM m) ->
  (inst -> a -> LangM m) ->
  Parser a ->
  IO ()
testTask gen f partial full p = do
  inst <- generate gen
  desc <- run (f inst)
  print desc
  str <- getLine
  case parse p "input" str of
    Left err -> print err
    Right value -> do
      putStrLn "---- Input ----"
      print value
      putStrLn "---- Partial ----"
      partialRes <- run (partial inst value)
      print partialRes
      putStrLn "---- Complete ----"
      completeRes <- run (full inst value)
      print completeRes

checkConfigWith :: (m ~ GenericReportT Language (IO ()) IO) => config -> (config -> LangM m) -> IO Bool
checkConfigWith conf check = isJust <$> run (check conf)

run :: (m ~ GenericReportT Language (IO ()) IO) => LangM m -> IO (Maybe ())
run thing = do
  (r,sayThing) <- runLangMReport (pure ()) (>>) thing
  sayThing German
  pure r
