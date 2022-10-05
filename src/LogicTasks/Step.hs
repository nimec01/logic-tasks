{-# language RecordWildCards #-}

module LogicTasks.Step where



import Config (StepConfig(..), StepInst(..), BaseConfig(..))
import Types
import Formula
import Util
import Resolution

import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.List (delete)
import Test.QuickCheck (Gen, elements)

import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  english,
  german,
  translate
  )




genStepInst :: StepConfig -> Gen StepInst
genStepInst StepConfig{ baseConf = BaseConfig{..}, ..} = do
    rChar <- elements usedLiterals
    rLit <- elements [Literal rChar, Not rChar]
    let
      restLits = delete rChar usedLiterals
    minLen1 <- elements [minClauseLength-1..maxClauseLength-1]
    minLen2 <- elements [minClauseLength-1..maxClauseLength-1]
    clause1 <- genClause (minLen1,maxClauseLength-1) restLits
    let
      lits1 = literals clause1
    clause2 <- tryGen (genClause (minLen2,maxClauseLength-1) restLits) 100
        (all (\lit -> opposite lit `notElem` lits1) .  literals)
    let
      litAddedClause1 = mkClause $ rLit : lits1
      litAddedClause2 = mkClause $ opposite rLit : literals clause2
    pure $ StepInst litAddedClause1 litAddedClause2 extraText



description :: OutputMonad m => StepInst -> LangM m
description StepInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die zwei folgenden Klauseln:"
      english "Consider the two following clauses:"
    indent $ code $ show clause1
    indent $ code $ show clause2

  paragraph $ translate $ do
    german "Resolvieren Sie die Klauseln und geben Sie die Resolvente an."
    english "Resolve the clauses and give the resulting resolvent."

  paragraph $ translate $ do
    german "Geben Sie das in dem Resolutionsschritt genutzte Literal und das Ergebnis in der folgenden Tupelform an: (Literal, Resolvente als ASCII-basierte Klausel)."
    english "Provide the literal used for the step and the resolvent in the following tuple form: (literal, resolvent as ASCII based clause)."

  paragraph $ translate $ do
    german "Beachten Sie dabei für die ASCII-Formel diese Legende:"
    english "Consider this key for the ASCII based formula:"

  paragraph $ indent $ do
    text "Negation:"
    code "~"

  paragraph $ indent $ do
    translate $ do
      german "Oder:"
      english "Or:"
    code "\\/"

  paragraph $ indent $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "A valid solution could look like this: "
    code "(A, ~B \\/ C)"

  paragraph $ text (fromMaybe "" addText)



verifyStatic :: OutputMonad m => StepInst -> LangM m
verifyStatic StepInst{..}
    | any isEmptyClause [clause1, clause2] =
        refuse $ indent $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | not $ resolvable clause1 clause2 =
        refuse $ indent $ translate $ do
          german "Die Klauseln sind nicht resolvierbar."
          english "The clauses are not resolvable."

    | otherwise = pure()



verifyQuiz :: OutputMonad m => StepConfig -> LangM m
verifyQuiz StepConfig{..} = checkBaseConf baseConf



start :: (Literal, Clause)
start = (Literal ' ', mkClause [])



partialGrade :: OutputMonad m => StepInst -> (Literal, Clause) -> LangM m
partialGrade StepInst{..} sol = do
  prevent (not (fst sol `Set.member` availLits)) $
    translate $ do
      german "Das gewählte Literal kommt in einer der Klauseln vor?"
      english "The chosen literal is contained in any of the clauses?"

  preventWithHint (not $ null extra)
    (translate $ do
      german "Resolvente besteht aus bekannten Literalen?"
      english "Resolvent contains only known literals?"
    )
    (paragraph $ do
      translate $ do
        german "In der Resolvente sind unbekannte Literale enthalten. Diese Literale sind falsch: "
        english "The resolvent contains unknown literals. These literals are incorrect:"
      itemizeM $ map (text . show) extra
    )

  where
     availLits = Set.fromList (literals clause1) `Set.union` Set.fromList (literals clause2)
     solLits = Set.fromList $ literals $ snd sol
     extra = Set.toList (solLits `Set.difference` availLits)



completeGrade :: OutputMonad m => StepInst -> (Literal, Clause) -> LangM m
completeGrade StepInst{..} sol =
    case resolve clause1 clause2 (fst sol) of
        Nothing -> refuse $ indent $ translate $ do
                     german "Mit diesem Literal kann kein Schritt durchgeführt werden!"
                     english "This literal can not be used for a resolution step!"

        Just solClause -> if solClause == snd sol
                            then pure()
                            else refuse $ indent $ translate $ do
                                   german "Resolvente ist nicht korrekt."
                                   english "Resolvent is not correct."
