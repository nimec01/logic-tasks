{-# language RecordWildCards #-}

module LogicTasks.Fill where


import Config
import Printing
import Table
import Types
import Formula

import Data.Maybe (fromMaybe)

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






