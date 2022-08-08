{-# language RecordWildCards #-}

module LogicTasks.Max where




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
  translate,
  refuse
  )





description :: OutputMonad m => MaxInst -> LangM m
description MaxInst{..} = do
  paragraph $ translate $ do
    german "Betrachten Sie die folgende Wahrheitstafel:"
    english "Consider the following truth table:"
    -- PDoc $ nest 4 $ pretty (getTable cnf)

  paragraph $ translate $ do
    german "Geben Sie eine zu der Tafel passende Formel in konjunktiver Normalform an. Verwenden Sie dazu Max-Terme."
    english "Provide a formula in conjunctive normal form, that corresponds to the table. Use maxterms to do this."

  paragraph $ translate $ do
    german "Reichen Sie ihre Lösung als ascii-basierte Formel ein."
    english "Provide the solution as an ascii based formula."

  paragraph $ translate $ do
    german "Beachten Sie dabei die folgende Legende:"
    english "Use the following key:"

  paragraph $ do
    translate $ do
      german "Negation"
      english "negation"
    text ": ~"

  paragraph $ do
    translate $ do
      german "oder"
      english "or"
    text ": \\/"

  paragraph $ do
    translate $ do
      german "und"
      english "and"
    text ": /\\"

  paragraph $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "A valid solution could look like this: "
    -- PDoc $ pretty $ mkCnf $ [mkClause [Literal 'A', Not 'B'], mkClause [Not 'C', Not 'D']]

  paragraph $ text (fromMaybe "" addText)





verifyStatic :: OutputMonad m => MaxInst -> Maybe (LangM m)
verifyStatic MaxInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        Just $ translate $ do
          german "Geben Sie bitte eine nicht-leere Formel an."
          english "Please give a non empty formula."

    | otherwise = Nothing




verifyQuiz :: OutputMonad m => MinMaxConfig -> LangM m
verifyQuiz MinMaxConfig{..}


    | isOutside 0 100 low || isOutside 0 100 high =
        refuse $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liegt nicht zwischen 0 und 100 Prozent."
          english "The given restriction on true entries are not in the range of 0 to 100 percent."

    | low > high =
        refuse $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liefert keine gültige Reichweite."
          english "The given restriction on true entries are not a valid range."

    | otherwise = checkCnfConf cnfConf

  where
    (low,high) = fromMaybe (0,100) percentTrueEntries



start :: Cnf
start = mkCnf []



partialGrade :: OutputMonad m => MaxInst -> Cnf -> Maybe (LangM m)
partialGrade MaxInst{..} sol
    | not (null extra) =
        Just $ paragraph $ do
          translate $ do
            german "Es sind unbekannte Literale enthalten. Diese Literale kommen in der korrekten Lösung nicht vor: "
            english "Your submission contains unknown literals. These do not appear in a correct solution: "
          itemizeM $ map (text . show) extra

    | not (null missing) =
        Just $ paragraph $ do
          translate $ do
            german "Es fehlen Literale. Fügen Sie Diese Literale der Abgabe hinzu: "
            english "Some literals are missing. Add these literals to your submission: "
          itemizeM $ map (text . show) missing

    | not  (all (\c -> amount c == length corLits) (getClauses sol)) =
        Just $ translate $ do
          german "Nicht alle Klauseln sind Maxterme!"
          english "Not all clauses are maxterms!"

    | solLen < corrLen =
        Just $ paragraph $ do
          translate $ do
            german "Die angegebene Formel enthält zu wenige Maxterme. Fügen sie "
            english "The formula does not contain enough maxterms. Add "
          text diff
          translate $ do
            german " hinzu!"
            english "!"

    | solLen > corrLen =
        Just $ paragraph $ do
          translate $ do
            german " Die angegebene Formel enthält zu viele Maxterme. Entfernen sie "
            english "The formula contains too many maxterms. Remove "
          text $ diff ++ "!"

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





completeGrade :: OutputMonad m => MaxInst -> Cnf -> Maybe (LangM m)
completeGrade MaxInst{..} sol

    | not (null diff) =
        Just $ paragraph $ do
          translate $ do
            german "Es existieren falsche Einträge in den folgenden Tabellenspalten: "
            english "The following rows are not correct: "
          itemizeM $ map (text . show) diff

    | otherwise = Nothing
  where
    solTable = getTable sol
    (_,diff) = pairwiseCheck (zip3 (readEntries solTable) (readEntries $ getTable cnf) [1..])






