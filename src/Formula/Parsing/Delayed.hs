{-# LANGUAGE FlexibleContexts #-}
module Formula.Parsing.Delayed (Delayed, delayed, withDelayed, parseDelayedAndThen) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Formula.Parsing (Parse(..))
import ParsingHelpers (fully)

import Control.Monad.Output (LangM, english, german, OutputMonad)

import LogicTasks.Helpers (reject)

newtype Delayed a = Delayed String

delayed :: String -> Delayed a
delayed = Delayed

parseDelayed :: Parser a -> Delayed a -> Either ParseError a
parseDelayed = parseDelayedRaw

parseDelayedRaw :: Parser b -> Delayed a -> Either ParseError b
parseDelayedRaw p (Delayed str) = parse p "(answer string)" str

withDelayed :: OutputMonad m => (a -> LangM m) -> Parser a -> Delayed a -> LangM m
withDelayed grade p d =
  case parseDelayed (fully p) d of
    Left err -> reject $ do
      english $ show err
      german $ show err
    Right x -> grade x

parseDelayedAndThen :: (OutputMonad m, Parse a) => Parser () -> (a -> LangM m) -> Delayed a -> LangM m
parseDelayedAndThen fallBackParser whatToDo delayedAnswer =
  case parseDelayed (fully parser) delayedAnswer of
    Right res -> whatToDo res
    Left err -> reject $ case parseDelayedRaw (fully fallBackParser) delayedAnswer of
      Left _ -> do
        german $ show err
        english $ show err
      Right () -> do
        german $  unlines
          [ "Ihre Abgabe konnte nicht gelesen werden." {- german -}
          , "Bitte prüfen Sie, ob die Anordnung der Symbole den Regeln zur Wohlaufgebautheit der Eingaben genügt." {- german -}
          , "Insbesondere sollten Sie genügend Klammern benutzen." {- german -}
          ]
        english $ unlines
          [ "Unable to read solution."
          , "Please make sure that the arrangement of symbols adheres to the rules for well-formedness of inputs."
          , "In particular, you should use enough parentheses."
          ]
