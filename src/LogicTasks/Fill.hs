{-# language RecordWildCards #-}

module LogicTasks.Fill where


import Config
import Table
import Types
import Formula
import Util

import Data.Maybe (fromMaybe, fromJust)

import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  english,
  german,
  translate
  )





description :: OutputMonad m => FillInst -> LangM m
description FillInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die folgende Formel:"
      english "Consider the following formula:"
    indent $ code $ "F = " ++ show cnf

  paragraph $ do
    translate $ do
      german "Füllen Sie in der zugehörigen Wahrheitstafel alle Lücken mit einem passenden Wahrheitswert (Wahr oder Falsch)."
      english "Fill all blanks in the corresponding truth table with truth values (True or False)."
    indent $ code $ show $ gapsAt (getTable cnf) missing

  paragraph $ translate $ do
    german "Geben Sie als Lösung eine Liste der fehlenden Wahrheitswerte an, wobei das erste Element der Liste der ersten Lücke von oben entspricht, das zweite Element der zweiten Lücke, etc."
    english "Provide the solution as a list of truth values. The first element of the list fills the first blank from the top, the second element fills the second blank, etc."

  paragraph $ translate $ do
    german "Die Eingabe der Werte kann binär (0 = falsch, 1 = wahr), ausgeschrieben (wahr, falsch) oder als Kurzform (f, w) erfolgen."
    english "Values can be submitted in binary form (0 = false, 1 = true), by entering the entire word (true, false) or by giving a shorthand (f or t)."

  paragraph $ indent $ do
    translate $ do
      german "Ein Lösungsversuch im Fall von vier Lücken könnte beispielsweise so aussehen:"
      english "A valid solution for four blanks could look like this:"
    code "[f,t,t,t]"

  paragraph $ text (fromMaybe "" addText)




verifyStatic :: OutputMonad m => FillInst -> LangM m
verifyStatic FillInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-leere Formel an."
          english "Please give a non empty formula."

    | any (> 2^length (atomics cnf)) missing =
        refuse $ indent $ translate $ do
          german "Mindestens ein gegebener Index ist zu hoch."
          english "At least one given index is too high."

    | any (<= 0) missing =
        refuse $ indent $ translate $ do
          german "Mindestens ein gegebener Index ist null oder negativ."
          english "At least one given index is zero or negative."

    | null missing =
        refuse $ indent $ translate $ do
          german "Es muss mindestens eine Lücke geben."
          english "At least one blank has to be specified."

    | otherwise = pure()




verifyQuiz :: OutputMonad m => FillConfig -> LangM m
verifyQuiz FillConfig{..}


    | isOutside 1 100 percentageOfGaps =
        refuse $ indent $ translate$ do
          german "Der prozentuale Anteil an Lücken muss zwischen 1 und 100 liegen."
          english "The percentile of gaps has to be set between 1 and 100."

    | isOutside 0 100 low || isOutside 0 100 high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liegt nicht zwischen 0 und 100 Prozent."
          english "The given restriction on true entries are not in the range of 0 to 100 percent."


    | low > high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liefert keine gültige Reichweite."
          english "The given restriction on true entries are not a valid range."

    | otherwise = checkCnfConf cnfConf

  where
    (low,high) = fromMaybe (0,100) percentTrueEntries



start :: [TruthValue]
start = []


partialGrade :: OutputMonad m => FillInst -> [TruthValue] -> LangM m
partialGrade FillInst{..} sol = do
  preventWithHint (solLen > acLen)
    (translate $ do
      german "Lösung überschreitet nicht maximale Anzahl Werte?"
      english "Solution does not exceed maximum possible number of values?"
    )
    (translate $ do
      german $ "Lösung enthält zu viele Werte. Es " ++ ger ++" entfernt werden."
      english $ "Solution contains too many values. Please remove " ++ eng ++ " to proceed."
    )

  preventWithHint (acLen > solLen)
    (translate $ do
      german "Lösung hat genügend Werte?."
      english "Solution contains enough values?."
    )
    (translate $ do
      german $ "Lösung enthält zu wenige Werte. Es " ++ ger ++ " hinzugefügt werden."
      english $ "Solution does not contain enough values. Please add " ++ eng ++ " to proceed."
    )

  where
    acLen = length missing
    solLen = length sol
    distance = abs (solLen - acLen)
    display = show distance
    (ger, eng) = if distance == 1
      then ("muss " ++ display ++ " Wert", display ++ " value")
      else ("müssen " ++ display ++ " Werte", display ++ " values")



completeGrade :: OutputMonad m => FillInst -> [TruthValue] -> LangM m
completeGrade FillInst{..} sol = do
  preventWithHint (not $ null diff)
    (translate $ do
      german "Lösung ist korrekt?"
      english "Solution is korrekt?"
    )
    (translate $ do
      german $ "Die Lösung beinhaltet " ++ display ++ " Fehler."
      english $ "Your solution contains " ++ display ++ " mistakes."
    )

  where
    table = getTable cnf
    correct = [ fromJust (readEntries table !! i) | i <- map (\x -> x-1) missing]
    boolSol = map truth sol
    zipped = zip3 boolSol correct [1..]
    (_,diff) = pairwiseCheck zipped
    display = show (length diff)
