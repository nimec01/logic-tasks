{-# language RecordWildCards #-}

module LogicTasks.Fill where


import Config
import Printing
import Table
import Types
import Formula
import Util

import Data.List (nub)
import Data.Maybe (fromMaybe, fromJust)

import Text.PrettyPrint.Leijen.Text




description :: FillInst -> [Either MText Doc]
description FillInst{..} =
              [ Left [(DE, "Betrachten Sie die folgende Formel in konjunktiver Normalform:")
                     ,(UK, "Consider the following formula in conjunctive normal form:")
                     ]
              , Right line
              , Right $ nest 4 $ myText "F = " <+> pretty cnf
              , Right line
              , Left [(DE, "Füllen Sie in der zugehörigen Wahrheitstafel alle Lücken mit einem passenden Wahrheitswert (True oder False).")
                     ,(UK, "Fill all blanks in the corresponding truth table with either True or False.")
                     ]
              , Left [(DE, "Geben Sie als Lösung eine Liste der fehlenden Werte an,")
                     ,(UK, "Provide the solution as a list.")
                     ]
              , Left [(DE, "wobei das erste Element der Liste der ersten Zeile entspricht, das zweite Element der zweiten Zeile, etc.")
                     ,(UK, "The first element of the list fills the first blank, the second element fills the second blank, etc.")
                     ]
              , Right $ myText (fromMaybe "" addText)
              , Right line
              , Right $ nest 4 $ pretty (gapsAt (getTable cnf) missing)
              ]




verifyStatic :: FillInst -> Maybe MText
verifyStatic FillInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        Just [ (DE, "Geben Sie bitte eine nicht-leere Formel an.")
             , (UK, "Please give a non empty formula.")
             ]

    | any (> 2^length (atomics cnf)) missing =
        Just [ (DE, "Mindestens ein gegebener Index ist zu hoch.")
             , (UK, "At least one given index is too high.")
             ]

    | any (<= 0) missing =
        Just [ (DE, "Mindestens ein gegebener Index ist null oder negativ.")
             , (UK, "At least one given index is zero or negative.")
             ]

    | null missing =
        Just [ (DE, "Es muss mindestens eine Lücke geben.")
             , (UK, "At least one blank has to be specified.")
             ]

    | otherwise = Nothing




verifyQuiz :: FillConfig -> Maybe MText
verifyQuiz FillConfig{..}


    | isOutside 1 100 percentageOfGaps =
        Just [ (DE, "Der prozentuale Anteil an Lücken muss zwischen 1 und 100 liegen.")
             , (UK, "The percentile of gaps has to be set between 1 and 100.")
             ]

    | isOutside 0 100 low || isOutside 0 100 high =
        Just [ (DE, "Die Beschränkung der Wahr-Einträge liegt nicht zwischen 0 und 100 Prozent.")
             , (UK, "The given restriction on true entries are not in the range of 0 to 100 percent.")
             ]

    | low > high =
        Just [ (DE, "Die Beschränkung der Wahr-Einträge liefert keine gültige Reichweite.")
             , (UK, "The given restriction on true entries are not a valid range.")
             ]

    | otherwise = checkCnfConf cnfConf

  where
    (low,high) = fromMaybe (0,100) percentTrueEntries






partialGrade :: FillInst -> [Int] -> Maybe MText
partialGrade FillInst{..} sol
    | not (null notBin) =
        Just [(DE, "Lösung enthält Werte die nicht 0 oder 1 sind. Diese sind keine Wahrheitswerte: " ++ show notBin)
             ,(UK, "Your Solution contains values which are not 0 or 1. The following are not truth values: " ++ show notBin)
             ]

    | solLen > acLen =
        Just [(DE, "Lösung enthält zu viele Werte. Es " ++ ger ++" entfernt werden.")
             ,(UK, "Solution contains too many values. Please remove " ++ eng ++ " to proceed.")
             ]

    | acLen > solLen =
        Just [(DE, "Lösung enthält zu wenige Werte. Es " ++ ger ++ " hinzugefügt werden.")
             , (UK, "Solution does not contain enough values. Please add " ++ eng ++ " to proceed.")
             ]

    | otherwise = Nothing


  where
    acLen = length missing
    solLen = length sol
    distance = abs (solLen - acLen)
    display = show distance
    notBin = nub $ filter (> 1) sol
    (ger, eng) = if distance == 1
      then ( "muss " ++ display ++ " Wert", display ++ " value")
      else ("müssen " ++ display ++ " Werte.", display ++ " values")



completeGrade :: FillInst -> [Int] -> Maybe MText
completeGrade FillInst{..} sol

    | not (null diff) =
        Just [(DE, "Die Lösung beinhaltet " ++ display ++ " Fehler.")
             ,(UK, "Your solution contains " ++ display ++ " mistakes.")
             ]
    | otherwise = Nothing

  where
    table = getTable cnf
    correct = [ fromJust (readEntries table !! i) | i <- map (\x -> x-1) missing]
    boolSol = map toEnum sol
    zipped = zip3 boolSol correct [1..]
    (_,diff) = pairwiseCheck zipped
    display = show (length diff)



