{-# language RecordWildCards #-}

module LogicTasks.Decide where




import Config
import Printing
import Table
import Types
import Formula
import Util

import Data.List (nub)
import Data.Maybe (fromMaybe)


import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  english,
  german,
  translate
  )



description :: OutputMonad m => DecideInst -> LangM m
description DecideInst{..} = do
  paragraph $ translate $ do
    english "Consider the following formula:"
    german "Betrachten Sie die folgende Formel:"
  --PDoc $ nest 4 $ myText "F = " <+> pretty cnf
  paragraph $ translate $ do
    english "Find all faulty entries in the last column of the following truth table."
    german "Finden Sie alle fehlerhaften Wahrheitswerte in der letzen Spalte der folgenden Wahrheitstafel."
  --PDoc $ nest 4 $ pretty (flipAt (getTable cnf) changed)
  paragraph $ translate $ do
    english "Provide the solution as a list of indices of the faulty rows. The indices start with 1."
    german "Geben Sie die Lösung als eine Liste der Indizes der fehlerhaften Zeilen an. Die Indizes beginnen dabei mit der 1."

  paragraph $ do
    translate $ do
      english "A valid solution could look like this: "
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
    text "[1,4,5]"

  paragraph $ text (fromMaybe "" addText)





verifyStatic :: OutputMonad m => DecideInst -> Maybe (LangM m)
verifyStatic DecideInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        Just $ translate $ do
          english "Please give a non empty formula."
          german "Geben Sie bitte eine nicht-leere Formel an."



    | any (> 2^length (atomics cnf)) changed =
        Just $ translate $ do
          english "At least one given index is too high."
          german "Mindestens ein gegebener Index ist zu hoch."


    | any (<= 0) changed =
        Just $ translate $ do
          english "At least one given index is zero or negative."
          german "Mindestens ein gegebener Index ist null oder negativ."


    | null changed =
        Just $ translate $ do
          english "At least one mistake has to be specified."
          german "Es muss mindestens eine Änderung geben."

    | otherwise = Nothing




verifyQuiz :: OutputMonad m => DecideConfig -> Maybe (LangM m)
verifyQuiz DecideConfig{..}


    | isOutside 1 100 percentageOfChanged =
        Just $ translate $ do
          english "The percentile of mistakes has to be set between 1 and 100."
          german "Der prozentuale Anteil an Fehlern muss zwischen 1 und 100 liegen."

    | otherwise = checkCnfConf cnfConf



start :: [Int]
start = []





partialGrade :: OutputMonad m =>  DecideInst -> [Int] -> LangM m
partialGrade DecideInst{..} sol = do
  preventCont (solLen > acLen)
    (translate $ do
      german $ "Lösung enthält nicht zu viele Indices?"
      english $ "Solution does not contains too many indices?")
    (translate $ do
      german $ "Lösung enthält zu viele Indices. Es " ++ ger ++" entfernt werden."
      english $ "Solution contains too many indices. Please remove " ++ eng ++ " to proceed.")


  prevent (acLen > solLen) $ translate $ do
    german $ "Lösung enthält genügend Indices?"
    english $ "Solution contains enough indices?"

  paragraph $ translate $ do
    german $ "Lösung enthält zu wenige Indices. Es " ++ ger ++ " hinzugefügt werden."
    english $ "Solution does not contain enough indices. Please add " ++ eng ++ " to proceed."



  where
    acLen = length $ nub changed
    solLen = length $ nub sol
    distance = abs (solLen - acLen)
    display = show distance
    (ger, eng) = if distance == 1
    then ( "muss " ++ display ++ " spezifischer Wert", display ++ " unique value")
    else ("müssen " ++ display ++ " spezifische Werte.", display ++ " unique values")




completeGrade :: OutputMonad m => DecideInst -> [Int] -> LangM m
completeGrade DecideInst{..} sol = do
  prevent (diff /= 0) $ translate $ do
          german $ "Die Lösung beinhaltet " ++ display ++ " Fehler."
          english $ "Your solution contains " ++ display ++ " mistakes."

  where
    nubSol = nub sol
    diff = length $ filter (`notElem` changed) nubSol
    display = show diff





