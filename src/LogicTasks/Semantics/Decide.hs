{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Decide where


import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  )
import Data.List (nub)
import Test.QuickCheck (Gen)

import Config (BaseConfig(..), CnfConfig(..), DecideConfig(..), DecideInst(..))
import Formula.Util (isEmptyCnf, hasEmptyClause)
import Formula.Table (flipAt, readEntries)
import Formula.Types (atomics, availableLetter, genCnf, getTable, literals)
import Util (checkCnfConf, isOutside, preventWithHint, remove)
import LogicTasks.Helpers (extra)




genDecideInst :: DecideConfig -> Gen DecideInst
genDecideInst DecideConfig{cnfConf = CnfConfig {baseConf = BaseConfig{..}, ..}, ..} = do
    cnf <- getCnf
    let
      tableLen = length $ readEntries $ getTable cnf
      mistakeCount = max (tableLen * percentageOfChanged `div` 100) 1
    mistakes <- remove (tableLen - mistakeCount) [1..tableLen]
    pure $ DecideInst cnf mistakes extraText
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals



description :: OutputMonad m => DecideInst -> LangM m
description DecideInst{..} = do
  paragraph $ do
    translate $ do
      english "Consider the following formula:"
      german "Betrachten Sie die folgende Formel:"
    indent $ code $ availableLetter (literals cnf) : " = " ++ show cnf
    pure ()
  paragraph $ do
    translate $ do
      english "Find all faulty entries in the last column of the following truth table."
      german "Finden Sie alle fehlerhaften Wahrheitswerte in der letzten Spalte der folgenden Wahrheitstafel."
    indent $ code $ show (flipAt (getTable cnf) changed)
    pure ()
  paragraph $ translate $ do
    english  "Provide the solution as a list of indices of the faulty rows. The row with 0 for all atomic formulas counts as row 1."
    german  "Geben Sie die Lösung als eine Liste der Indizes der fehlerhaften Zeilen an. Dabei zählt die Zeile mit 0 für alle atomaren Formeln als Zeile 1."

  paragraph $ indent $ do
    translate $ do
      english "A valid solution could look like this: "
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
    code "[1,4,5]"
    pure ()
  extra addText
  pure ()


verifyStatic :: OutputMonad m => DecideInst -> LangM m
verifyStatic DecideInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        refuse $ indent $ translate $ do
          english "Please give a non empty formula."
          german "Geben Sie bitte eine nicht-leere Formel an."



    | any (> 2^length (atomics cnf)) changed || any (<=0) changed =
        refuse $ indent $ translate $ do
          english "At least one of the given indices does not exist."
          german "Mindestens einer der angegebenen Indizes existiert nicht."



    | null changed =
        refuse $ indent $ translate $ do
          english "At least one mistake has to be specified."
          german "Es muss mindestens eine Änderung geben."

    | otherwise = pure ()



verifyQuiz :: OutputMonad m => DecideConfig -> LangM m
verifyQuiz DecideConfig{..}
    | isOutside 1 100 percentageOfChanged =
        refuse $ indent $ translate $ do
          english "The percentile of mistakes has to be set between 1 and 100."
          german "Der prozentuale Anteil an Fehlern muss zwischen 1 und 100 liegen."

    | otherwise = checkCnfConf cnfConf



start :: [Int]
start = []



partialGrade :: OutputMonad m =>  DecideInst -> [Int] -> LangM m
partialGrade DecideInst{..} sol = do
  preventWithHint (null sol)
    (translate $ do
      german "Lösung enthält Indizes?"
      english "Solution contains indices?"
    )
    (translate $ do
      german "Die Lösung beinhaltet mindestens einen Index."
      english "The solution contains at least one index."
    )

  preventWithHint (solLen > tableLen)
    (translate $ do
      german "Lösung überschreitet nicht Anzahl der Zeilen?"
      english "Solution does not exceed count of rows?"
    )
    (translate $ do
      german $ "Lösung enthält mehr Werte als es Zeilen gibt. Es " ++ gerLong ++" entfernt werden."
      english $ "Solution contains more values than rows. Please remove at least " ++ engLong ++ " to proceed."
    )


  pure ()
  where
    tableLen = length $ readEntries $ getTable cnf
    solLen = length sol
    diffToTable = abs (solLen - tableLen)
    (gerLong,engLong) = gerEng diffToTable
    gerEng diff = if diff == 1
        then ("muss mindestens" ++ display ++ " Wert", display ++ " value") -- no-spell-check
        else ("müssen mindestens" ++ display ++ " Werte", display ++ " values") -- no-spell-check
      where
        display = show diff

completeGrade :: OutputMonad m => DecideInst -> [Int] -> LangM m
completeGrade DecideInst{..} sol = do
  preventWithHint (solLen > acLen)
    (translate $ do
      german "Lösung enthält nicht zu viele Indizes?"
      english "Solution does not contain too many indices?"
    )
    (translate $ do
      german "Lösung enthält zu viele Indizes."
      english "Solution contains too many indices."
    )

  preventWithHint (acLen > solLen)
    (translate $ do
      german "Lösung enthält genügend Indizes?"
      english "Solution contains enough indices?"
    )
    (translate $ do
      german "Lösung enthält zu wenige Indizes."
      english "Solution does not contain enough indices."
    )

  preventWithHint (diff /= 0)
    (translate $ do
      german "Lösung ist korrekt?"
      english "Solution is correct?"
    )
    (translate $ do
      german $ "Die Lösung beinhaltet " ++ display ++ " Fehler."
      english $ "Your solution contains " ++ display ++ " mistakes."
    )

  pure ()
  where
    nubSol = nub sol
    diff = length $ filter (`notElem` changed) nubSol
    acLen = length $ nub changed
    solLen = length $ nub sol
    distance = abs (solLen - acLen)
    display = show distance

