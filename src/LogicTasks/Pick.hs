{-# language RecordWildCards #-}

module LogicTasks.Pick where




import Config (BaseConfig(..), CnfConfig(..), PickConfig(..), PickInst(..), Number(..))
import Printing
import Types
import Formula
import Util

import Data.Maybe (fromMaybe)


import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  english,
  german,
  translate
  )

import Text.PrettyPrint.Leijen.Text



description :: PickInst -> [ProxyDoc]
description PickInst{..} =
              [ PMult ("Betrachten Sie die folgende Formel:"
                     ,"Consider the following formula:"
                     )
              , PDoc line
              , PDoc $ nest 4 $ myText "F = " <+> pretty (cnfs !! (correct - 1))
              , PDoc line
              , PMult ("Welche der folgenden Wahrheitstafeln passt zu der Formel? Geben Sie die richtige Tafel durch ihre Nummer an."
                      ,"Which of these truth tables represents the formula? Specify the correct table by giving its number."
                      )
              , PDoc line
              , PDoc $ nest 4 $ showIndexedList 120 5 $ map getTable cnfs
              , Composite [ PMult ("Ein Lösungsversuch könnte beispielsweise so aussehen: "
                                  , "A valid solution could look like this: ")
                          , PDoc $ myText "1"
                          ]
              , PDoc line
              , PDoc $ myText (fromMaybe "" addText)
              ]



verifyStatic :: PickInst -> Maybe ProxyDoc
verifyStatic PickInst{..}
    | null cnfs =
        Just $ PMult ("Die Liste der Formeln ist leer."
                     ,"The list of formulae is empty."
                     )

    | mkCnf [] `elem` cnfs =
        Just $ PMult ("Für mindestens eine Formel kann keine Wahrheitstafel erstellt werden."
                     ,"For at least one given formula there is no corresponding truth table."
                     )

    | length cnfs < correct || correct <= 0 =
        Just $ PMult ("Der angegebene Index existiert nicht."
                     ,"The given index does not exist."
                     )

    | otherwise = Nothing



verifyQuiz :: OutputMonad m => PickConfig -> Maybe (LangM m)
verifyQuiz PickConfig{..}


    | amountOfOptions < 2 =
        Just $ translate $ do
          german "Es muss mindestens zwei Optionen geben."
          english "At least two options need to be given."

    | amountOfOptions > 4*2^ length (usedLiterals base) =
        Just $ translate $ do
          german "Die Anzahl Optionen übersteigt die Anzahl möglicher, unterschiedlicher Formeln."
          english "The amount of options is higher than the amount of possible, distinct formulae."

    | otherwise = checkCnfConf cnfConf

  where
    base = baseConf cnfConf



start :: Number
start = Number Nothing



partialGrade :: PickInst -> Number -> Maybe ProxyDoc
partialGrade _ _ = Nothing



completeGrade :: PickInst -> Number -> Maybe ProxyDoc
completeGrade PickInst{..} sol =
    case sol of Number Nothing -> Just $ PMult ("Es wurde kein Index angegeben."
                                               ,"You did not give an index."
                                               )
                Number (Just index) ->
                  if index == correct then Nothing
                                      else Just $ PMult ("Der gewählte Index ist falsch."
                                                        ,"You submitted the wrong index."
                                                        )
