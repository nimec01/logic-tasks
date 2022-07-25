{-# language RecordWildCards #-}

module LogicTasks.Min where




import Config
import Printing
import Table
import Types
import Formula
import Util

import Data.List ((\\))
import Data.Maybe (fromMaybe)


import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  english,
  german,
  translate
  )

import Text.PrettyPrint.Leijen.Text




description :: MinInst -> [ProxyDoc]
description MinInst{..} =
              [ PMult ("Betrachten Sie die folgende Wahrheitstafel:"
                      ,"Consider the following truth table:"
                      )
              , PDoc line
              , PDoc $ nest 4 $ pretty (getTable dnf)
              , PMult ("Geben Sie eine zu der Tafel passende Formel in disjunktiver Normalform an. Verwenden Sie dazu Min-Terme."
                      ,"Provide a formula in disjunctive normal form, that corresponds to the table. Use minterms to do this."
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
                                  ,"A valid solution could look like this: ")
                          , PDoc $ pretty $ mkDnf $ [mkCon [Literal 'A', Not 'B'], mkCon [Not 'C', Not 'D']]
                          ]
              , PDoc line
              , PDoc $ myText (fromMaybe "" addText)
              ]




verifyStatic :: MinInst -> Maybe ProxyDoc
verifyStatic MinInst{..}
    | isEmptyDnf dnf || hasEmptyCon dnf =
        Just $ PMult ("Geben Sie bitte eine nicht-leere Formel an."
                     ,"Please give a non empty formula."
                     )

    | otherwise = Nothing




verifyQuiz :: OutputMonad m => MinMaxConfig -> Maybe (LangM m)
verifyQuiz MinMaxConfig{..}


    | isOutside 0 100 low || isOutside 0 100 high =
        Just $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liegt nicht zwischen 0 und 100 Prozent."
          english "The given restriction on true entries are not in the range of 0 to 100 percent."

    | low > high =
        Just $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liefert keine gültige Reichweite."
          english "The given restriction on true entries are not a valid range."

    | otherwise = checkCnfConf cnfConf

  where
    (low,high) = fromMaybe (0,100) percentTrueEntries



start :: Dnf
start = mkDnf []



partialGrade :: MinInst -> Dnf -> Maybe ProxyDoc
partialGrade MinInst{..} sol
    | not (null extra) =
        Just $ Composite [ PMult ("Es sind unbekannte Literale enthalten. Diese Literale kommen in der korrekten Lösung nicht vor: "
                                 ,"Your submission contains unknown literals. These do not appear in a correct solution: "
                                 )
                         , PDoc $ pretty extra
                         ]

    | not (null missing) =
        Just $ Composite [ PMult ("Es fehlen Literale. Fügen Sie Diese Literale der Abgabe hinzu: "
                                 ,"Some literals are missing. Add these literals to your submission: "
                                 )
                         , PDoc $ pretty missing
                         ]

    | not  (all (\c -> amount c == length corLits) (getConjunctions sol)) =
        Just $ PMult ("Nicht alle Conjunktionen sind Minterme!"
                     ,"Not all conjunctions are minterms!"
                     )

    | solLen < corrLen =
        Just $ Composite [ PMult ("Die angegebene Formel enthält zu wenige Minterme. Fügen sie "
                                 ,"The formula does not contain enough minterms. Add "
                                 )
                         , PDoc $ pretty diff
                         , PMult (" hinzu!"
                                 ,"!")
                         ]

    | solLen > corrLen =
        Just $ Composite [ PMult ("Die angegebene Formel enthält zu viele Minterme. Entfernen sie "
                                 ,"The formula contains too many minterms. Remove "
                                 )
                         , PDoc $ pretty diff <+> myText "!"
                         ]

    | otherwise = Nothing

  where
    solLits = atomics sol
    corLits = atomics dnf
    extra = solLits \\ corLits
    missing = corLits \\ solLits
    table = getTable dnf
    corrLen = length $ filter (== Just True) (readEntries table)
    solLen = amount sol
    diff = show $ abs (solLen - corrLen)





completeGrade :: MinInst -> Dnf -> Maybe ProxyDoc
completeGrade MinInst{..} sol

    | not (null diff) =
        Just $ Composite [ PMult ("Es existieren falsche Einträge in den folgenden Tabellenspalten: "
                                 ,"The following rows are not correct: "
                                 )
                         , PDoc $ pretty diff
                         ]

    | otherwise = Nothing
  where
    solTable = getTable sol
    (_,diff) = pairwiseCheck (zip3 (readEntries solTable) (readEntries $ getTable dnf) [1..])






