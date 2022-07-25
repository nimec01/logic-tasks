{-# language RecordWildCards #-}

module LogicTasks.Step where



import Config (StepConfig(..), StepInst(..))
import Printing
import Types
import Formula
import Util
import Resolution

import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  english,
  german,
  translate
  )




description :: OutputMonad m => StepInst -> LangM m
description StepInst{..} = do
  paragraph $ translate $ do
    german "Betrachten Sie die zwei folgenden Klauseln:"
    english "Consider the two following clauses:"

    -- PDoc $ nest 4 $ pretty clause1

    -- PDoc $ nest 4 $ pretty clause2

  paragraph $ translate $ do
    german "Resolvieren Sie die Klauseln und geben Sie die Resolvente an."
    english "Resolve the clauses and give the resulting resolvent."

  paragraph $ translate $ do
    german "Geben Sie das in dem Resolutionsschritt genutzte Literal und das Ergebnis in der folgenden Tupelform an: (Literal, Resolvente als ASCII-basierte Klausel)."
    english "Provide the literal used for the step and the resolvent in the following tuple form: (literal, resolvent as ASCII based clause)."

  paragraph $ translate $ do
    german "Beachten Sie dabei für die ASCII-Formel diese Legende:"
    english "Consider this key for the ASCII based formula:"

  paragraph $ do
    translate $ do
      german "Negation"
      english "negation"
    text ": ~"

  paragraph $ do
    translate $ do
      german "oder"
      english "or"
    text ": \\/"

  paragraph $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "A valid solution could look like this: "
    text "(A, ~B \\/ C)"

  paragraph $ text (fromMaybe "" addText)



verifyStatic :: OutputMonad m => StepInst -> Maybe (LangM m)
verifyStatic StepInst{..}
    | any isEmptyClause [clause1, clause2] =
        Just $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | not $ resolvable clause1 clause2 =
        Just $ translate $ do
          german "Die Klauseln sind nicht resolvierbar."
          english "The clauses are not resolvable."

    | otherwise = Nothing



verifyQuiz :: OutputMonad m => StepConfig -> Maybe (LangM m)
verifyQuiz StepConfig{..} = checkBaseConf baseConf



start :: (Literal, Clause)
start = (Literal ' ', mkClause [])



partialGrade :: OutputMonad m => StepInst -> (Literal, Clause) -> Maybe (LangM m)
partialGrade StepInst{..} sol
    | not (fst sol `Set.member` availLits) =
        Just $ translate $ do
          german "Das gewählte Literal kommt in den Klauseln nicht vor."
          english "The chosen literal is not contained in any of the clauses."

    | not (null extra) =
        Just $ paragraph $ do
          translate $ do
            german "In der Resolvente sind unbekannte Literale enthalten. Diese Literale sind falsch: "
            english "The resolvent contains unknown literals. These literals are incorrect:"
          itemizeM $ map (text . show) extra

    | otherwise = Nothing

  where
     availLits = Set.fromList (literals clause1) `Set.union` Set.fromList (literals clause2)
     solLits = Set.fromList $ literals $ snd sol
     extra = Set.toList (solLits `Set.difference` availLits)



completeGrade :: OutputMonad m => StepInst -> (Literal, Clause) -> Maybe (LangM m)
completeGrade StepInst{..} sol =
    case resolve clause1 clause2 (fst sol) of
        Nothing -> Just $ translate $ do
                     german "Mit diesem Literal kann kein Schritt durchgeführt werden!"
                     english "This literal can not be used for a resolution step!"

        Just solClause -> if (solClause == snd sol)
                            then Nothing
                            else Just $ translate $ do
                                   german "Resolvente ist nicht korrekt."
                                   english "Resolvent is not correct."
