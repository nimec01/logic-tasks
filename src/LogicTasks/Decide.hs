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

import Text.PrettyPrint.Leijen.Text




description :: DecideInst -> [Either MText Doc]
description DecideInst{..} =
              [ Left  ("Betrachten Sie die folgende Formel in konjunktiver Normalform:"
                      ,"Consider the following formula in conjunctive normal form:"
                      )
              , Right line
              , Right $ nest 4 $ myText "F = " <+> pretty cnf
              , Right line
              , Left ("Finden Sie alle Fehlerhaften Wahrheitswerte in der folgenden Wahrheitstafel."
                     ,"Find all faulty entries in the following truth table."
                     )
              , Left ("Geben Sie die Lösung als eine Liste der Indices der fehlerhaften Zeilen an."
                     ,"Provide the solution as a list of indices of the faulty rows."
                     )
              , Right $ myText (fromMaybe "" addText)
              , Right line
              , Right $ nest 4 $ pretty (flipAt (getTable cnf) changed)
              ]




verifyStatic :: DecideInst -> Maybe MText
verifyStatic DecideInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        Just ("Geben Sie bitte eine nicht-leere Formel an."
             ,"Please give a non empty formula."
             )

    | any (> 2^length (atomics cnf)) changed =
        Just ("Mindestens ein gegebener Index ist zu hoch."
             ,"At least one given index is too high."
             )

    | any (<= 0) changed =
        Just ("Mindestens ein gegebener Index ist null oder negativ."
             ,"At least one given index is zero or negative."
             )

    | null changed =
        Just ("Es muss mindestens eine Änderung geben."
             ,"At least one mistake has to be specified."
             )

    | otherwise = Nothing




verifyQuiz :: DecideConfig -> Maybe MText
verifyQuiz DecideConfig{..}


    | isOutside 1 100 percentageOfChanged =
        Just ("Der prozentuale Anteil an Fehlern muss zwischen 1 und 100 liegen."
             ,"The percentile of mistakes has to be set between 1 and 100."
             )

    | otherwise = checkCnfConf cnfConf








partialGrade :: DecideInst -> [Int] -> Maybe MText
partialGrade DecideInst{..} sol
    | solLen > acLen =
        Just ("Lösung enthält zu viele Indices. Es " ++ ger ++" entfernt werden."
             ,"Solution contains too many indices. Please remove " ++ eng ++ " to proceed."
             )

    | acLen > solLen =
        Just ("Lösung enthält zu wenige Indices. Es " ++ ger ++ " hinzugefügt werden."
             ,"Solution does not contain enough indices. Please add " ++ eng ++ " to proceed."
             )

    | otherwise = Nothing


  where
    acLen = length $ nub changed
    solLen = length $ nub sol
    distance = abs (solLen - acLen)
    display = show distance
    (ger, eng) = if distance == 1
    then ( "muss " ++ display ++ " spezifischer Wert", display ++ " unique value")
    else ("müssen " ++ display ++ " spezifische Werte.", display ++ " unique values")




completeGrade :: DecideInst -> [Int] -> Maybe MText
completeGrade DecideInst{..} sol

    | diff /= 0 =
        Just ("Die Lösung beinhaltet " ++ display ++ " Fehler."
             ,"Your solution contains " ++ display ++ " mistakes."
             )
    | otherwise = Nothing

  where
    nubSol = nub sol
    diff = length $ filter (`notElem` changed) nubSol
    display = show diff





