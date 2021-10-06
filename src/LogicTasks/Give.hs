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




description :: GiveInst -> [ProxyDoc]
description GiveInst{..} =
              [ PMult ("Betrachten Sie die folgende Wahrheitstafel:"
                     ,"Consider the following truth table:"
                     )
              , PDoc line
              , PDoc $ nest 4 $ pretty (getTable cnf)
              , PMult ("Geben Sie eine zu der Tafel passende Formel in konjunktiver Normalform an. Verwenden Sie dazu Max-Terme."
                     ,"Provide a formula in conjunctive normal form, that corresponds to the table. Use maxterms to do this."
                     )
              , PMult ("Reichen Sie ihre Lösung als ascii-basierte Formel ein."
                     ,"Provide the solution as an ascii based formula."
                     )
              , PDoc line
              , PMult ("Beachten Sie dabei die folgende Legende:"
                     ,"Use the following key:"
                     )
              , PDoc line
              , Composite [ PMult ("Negation"
                                  ,"negation"
                                  )
                          , PDoc $ myText ": ~"
                          ]
              , Composite [ PMult ("oder"
                                  ,"or"
                                  )
                          , PDoc $ myText ": \\/"
                          ]
              , Composite [ PMult ("und"
                                  ,"and"
                                  )
                          , PDoc $ myText ": /\\"
                          ]
              , PDoc line
              , Composite [ PMult ("Ein Lösungsversuch könnte beispielsweise so aussehen: "
                                  , "A valid solution could look like this: ")
                          , PDoc $ pretty $ mkCnf $ [mkClause [Literal 'A', Not 'B'], mkClause [Not 'C', Not 'D']]
                          ]
              , PDoc line
              , PDoc $ myText (fromMaybe "" addText)
              ]




verifyStatic :: GiveInst -> Maybe MText
verifyStatic GiveInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        Just ("Geben Sie bitte eine nicht-leere Formel an."
             ,"Please give a non empty formula."
             )

    | otherwise = Nothing




verifyQuiz :: GiveConfig -> Maybe MText
verifyQuiz GiveConfig{..}


    | isOutside 0 100 low || isOutside 0 100 high =
        Just ("Die Beschränkung der Wahr-Einträge liegt nicht zwischen 0 und 100 Prozent."
             ,"The given restriction on true entries are not in the range of 0 to 100 percent."
             )

    | low > high =
        Just ("Die Beschränkung der Wahr-Einträge liefert keine gültige Reichweite."
             ,"The given restriction on true entries are not a valid range."
             )

    | otherwise = checkCnfConf cnfConf

  where
    (low,high) = fromMaybe (0,100) percentTrueEntries



start :: Cnf
start = mkCnf []



partialGrade :: GiveInst -> Cnf -> Maybe MText
partialGrade GiveInst{..} sol
    | not (null extra) =
        Just ("Es sind unbekannte Literale enthalten. "
                ++ "Diese Literale kommen in der korrekten Lösung nicht vor: "
                ++ show extra
             ,"Your submission contains unknown literals. "
                ++ "These do not appear in a correct solution: "
                ++ show extra
             )

    | not (null missing) =
        Just ("Es fehlen Literale. Fügen Sie Diese Literale der Abgabe hinzu: "
                ++ show missing
             ,"Some literals are missing. Add these literals to your submission: "
                ++ show missing
             )

    | not  (all (\c -> amount c == length corLits) (getClauses sol)) =
        Just ("Nicht alle Klauseln sind Maxterme!"
             ,"Not all clauses are maxterms!"
             )

    | solLen < corrLen =
        Just ("Die angegebene Formel enthält zu wenige Maxterme. Fügen sie " ++ diff ++ " hinzu!"
             ,"The formula does not contain enough maxterms. Add " ++ diff ++ "!"
             )

    | solLen > corrLen =
        Just ("Die angegebene Formel enthält zu viele Maxterme. Entfernen sie " ++ diff ++ "!"
             ,"The formula contains too many maxterms. Remove " ++ diff ++ "!"
             )

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
        Just ("Es existieren falsche Einträge in den folgenden Tabellenspalten: "
                ++ show diff
             ,"The following rows are not correct: "
                ++ show diff
             )
    | otherwise = Nothing

  where
    solTable = getTable sol
    (_,diff) = pairwiseCheck (zip3 (readEntries solTable) (readEntries $ getTable cnf) [1..])






