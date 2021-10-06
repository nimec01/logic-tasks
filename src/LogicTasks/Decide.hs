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




description :: DecideInst -> [ProxyDoc]
description DecideInst{..} =
              [ PMult ("Betrachten Sie die folgende Formel in konjunktiver Normalform:"
                      ,"Consider the following formula in conjunctive normal form:"
                      )
              , PDoc line
              , PDoc $ nest 4 $ myText "F = " <+> pretty cnf
              , PDoc line
              , PMult ("Finden Sie alle Fehlerhaften Wahrheitswerte in der folgenden Wahrheitstafel."
                      ,"Find all faulty entries in the following truth table."
                      )
              , PDoc line
              , PDoc $ nest 4 $ pretty (flipAt (getTable cnf) changed)
              , PMult ("Geben Sie die Lösung als eine Liste der Indices der fehlerhaften Zeilen an."
                      ,"Provide the solution as a list of indices of the faulty rows."
                      )
              , PDoc line
              , Composite [ PMult ("Ein Lösungsversuch könnte beispielsweise so aussehen: "
                                  , "A valid solution could look like this: ")
                          , PDoc $ myText "[1,4,5]"
                          ]
              , PDoc line
              , PDoc $ myText (fromMaybe "" addText)
              ]




verifyStatic :: DecideInst -> Maybe ProxyDoc
verifyStatic DecideInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        Just $ PMult ("Geben Sie bitte eine nicht-leere Formel an."
                     ,"Please give a non empty formula."
                     )

    | any (> 2^length (atomics cnf)) changed =
        Just $ PMult ("Mindestens ein gegebener Index ist zu hoch."
                     ,"At least one given index is too high."
                     )

    | any (<= 0) changed =
        Just $ PMult ("Mindestens ein gegebener Index ist null oder negativ."
                     ,"At least one given index is zero or negative."
                     )

    | null changed =
        Just $ PMult ("Es muss mindestens eine Änderung geben."
                     ,"At least one mistake has to be specified."
                     )

    | otherwise = Nothing




verifyQuiz :: DecideConfig -> Maybe ProxyDoc
verifyQuiz DecideConfig{..}


    | isOutside 1 100 percentageOfChanged =
        Just $ PMult ("Der prozentuale Anteil an Fehlern muss zwischen 1 und 100 liegen."
                     ,"The percentile of mistakes has to be set between 1 and 100."
                     )

    | otherwise = checkCnfConf cnfConf



start :: [Int]
start = []





partialGrade :: DecideInst -> [Int] -> Maybe ProxyDoc
partialGrade DecideInst{..} sol
    | solLen > acLen =
        Just $ PMult ("Lösung enthält zu viele Indices. Es " ++ ger ++" entfernt werden."
                     ,"Solution contains too many indices. Please remove " ++ eng ++ " to proceed."
                     )

    | acLen > solLen =
        Just $ PMult ("Lösung enthält zu wenige Indices. Es " ++ ger ++ " hinzugefügt werden."
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




completeGrade :: DecideInst -> [Int] -> Maybe ProxyDoc
completeGrade DecideInst{..} sol

    | diff /= 0 =
        Just $ PMult ("Die Lösung beinhaltet " ++ display ++ " Fehler."
                     ,"Your solution contains " ++ display ++ " mistakes."
                     )
    | otherwise = Nothing

  where
    nubSol = nub sol
    diff = length $ filter (`notElem` changed) nubSol
    display = show diff





