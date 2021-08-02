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




verify :: FillInst -> Maybe MText
verify FillInst{..}
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



