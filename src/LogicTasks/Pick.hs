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
  translate,
  refuse
  )




description :: OutputMonad m => PickInst -> LangM m
description PickInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die folgende Formel:"
      english "Consider the following formula:"
    indent $ code $ "F = " ++ show (cnfs !! (correct - 1))

  paragraph $ translate $ do
    german "Welche der folgenden Wahrheitstafeln passt zu der Formel? Geben Sie die richtige Tafel durch ihre Nummer an."
    english "Which of these truth tables represents the formula? Specify the correct table by giving its number."
  --indent $ code $ showIndexedList 120 5 $ map getTable cnfs

  paragraph $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "A valid solution could look like this: "
    text "1"

  paragraph $ text (fromMaybe "" addText)




verifyStatic :: OutputMonad m => PickInst -> LangM m
verifyStatic PickInst{..}
    | null cnfs =
        refuse $ indent $ translate $ do
          german "Die Liste der Formeln ist leer."
          english "The list of formulae is empty."

    | mkCnf [] `elem` cnfs =
        refuse $ indent $ translate $ do
          german "Für mindestens eine Formel kann keine Wahrheitstafel erstellt werden."
          english "For at least one given formula there is no corresponding truth table."

    | length cnfs < correct || correct <= 0 =
        refuse $ indent $ translate $ do
          german "Der angegebene Index existiert nicht."
          english "The given index does not exist."

    | otherwise = pure()



verifyQuiz :: OutputMonad m => PickConfig -> LangM m
verifyQuiz PickConfig{..}

    | amountOfOptions < 2 =
        refuse $ indent $ translate $ do
          german "Es muss mindestens zwei Optionen geben."
          english "At least two options need to be given."

    | amountOfOptions > 4*2^ length (usedLiterals base) =
        refuse $ indent $ translate $ do
          german "Die Anzahl Optionen übersteigt die Anzahl möglicher, unterschiedlicher Formeln."
          english "The amount of options is higher than the amount of possible, distinct formulae."

    | otherwise = checkCnfConf cnfConf

  where
    base = baseConf cnfConf



start :: Number
start = Number Nothing



partialGrade :: OutputMonad m => PickInst -> Number -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => PickInst -> Number -> LangM m
completeGrade PickInst{..} sol =
    case sol of Number Nothing -> refuse $ indent $ translate $ do
                                    german "Es wurde kein Index angegeben."
                                    english "You did not give an index."
                Number (Just index) ->
                  if index == correct then pure()
                                      else refuse $ indent $ translate $ do
                                             german "Der gewählte Index ist falsch."
                                             english "You submitted the wrong index."

