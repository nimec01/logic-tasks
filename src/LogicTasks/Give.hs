{-# language RecordWildCards #-}

module LogicTasks.Give where




import Config
import Printing
import Table
import Types
import Formula
import Util

import Data.List ((\\))
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.Leijen.Text




description :: GiveInst -> [Either MText Doc]
description GiveInst{..} =
              [ Left [ (DE, "Betrachten Sie die folgende Wahrheitstafel:")
                     , (UK, "Consider the following truth table:")
                     ]
              , Right line
              , Right $ nest 4 $ myText "F = " <+> pretty (getTable cnf)
              , Right line
              , Left [ (DE, "Geben Sie eine zu der Tafel passende Formel in konjunktiver Normalform an. Verwenden Sie dazu Max-Terme.")
                     , (UK, "Provide a formula in conjunctive normal form, that corresponds to the table. Use maxterms to do this.")
                     ]
              , Left [ (DE, "Reichen Sie ihre Lösung als ascii-basierte Formel ein.")
                     , (UK, "Provide the solution as an ascii based formula.")
                     ]
              , Left [ (DE, "Beachten Sie dabei die folgende Legende:")
                     , (UK, "Use the following key:")
                     ]
              , Right line
              , Left [ (DE, "Negation")
                     , (UK, "negation")
                     ]
              , Right $ myText ": ~"
              , Left [ (DE, "oder")
                     , (UK, "or")
                     ]
              , Right $ myText ": \\/"
              , Left [ (DE, "und")
                     , (UK, "and")
                     ]
              , Right $ myText ": /\\"
              , Right line
              , Right $ myText (fromMaybe "" addText)
              ]




verifyStatic :: GiveInst -> Maybe MText
verifyStatic GiveInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        Just [ (DE, "Geben Sie bitte eine nicht-leere Formel an.")
             , (UK, "Please give a non empty formula.")
             ]

    | otherwise = Nothing




verifyQuiz :: GiveConfig -> Maybe MText
verifyQuiz GiveConfig{..}


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







partialGrade :: GiveInst -> Cnf -> Maybe MText
partialGrade GiveInst{..} sol
    | not (null extra) =
        Just [ (DE, "Es sind unbekannte Literale enthalten. "
                ++ "Diese Literale kommen in der korrekten Lösung nicht vor: "
                ++ show extra)
             , (UK, "Your submission contains unknown literals. "
                ++ "These do not appear in a correct solution: "
                ++ show extra)
             ]

    | not (null missing) =
        Just [ (DE, "Es fehlen Literale. Fügen Sie Diese Literale der Abgabe hinzu: "
                ++ show missing)
             , (UK, "Some literals are missing. Add these literals to your submission: "
                ++ show missing)
             ]

    | not  (all (\c -> amount c == length corLits) (getClauses sol)) =
        Just [ (DE, "Nicht alle Klauseln sind Maxterme!")
             , (UK, "Not all clauses are maxterms!")
             ]

    | solLen < corrLen =
        Just [ (DE, "Die angegebene Formel enthält zu wenige Maxterme. Fügen sie " ++ diff ++ " hinzu!")
             , (UK, "The formula does not contain enough maxterms. Add " ++ diff ++ "!")
             ]

    | solLen > corrLen =
        Just [ (DE, "Die angegebene Formel enthält zu viele Maxterme. Entfernen sie " ++ diff ++ "!")
             , (UK, "The formula contains too many maxterms. Remove " ++ diff ++ "!")
             ]

    | otherwise = Nothing

  where
    solLits = atomics sol
    corLits = atomics cnf
    extra = solLits \\ corLits
    missing = corLits \\ solLits
    table = getTable cnf
    corrLen = length $ filter (== Just False) (readEntries table)
    solLen = amount sol
    diff = show $ abs (solLen - corrLen)





completeGrade :: GiveInst -> Cnf -> Maybe MText
completeGrade GiveInst{..} sol

    | not (null diff) =
        Just [ (DE, "Es existieren falsche Einträge in den folgenden Tabellenspalten: "
                ++ show diff)
             , (UK, "The following rows are not correct: "
                ++ show diff)
             ]
    | otherwise = Nothing

  where
    solTable = getTable sol
    (_,diff) = pairwiseCheck (zip3 (readEntries solTable) (readEntries $ getTable cnf) [1..])






