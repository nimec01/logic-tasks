{-# language RecordWildCards #-}

module LogicTasks.Pick where




import Config (BaseConfig(..), CnfConfig(..), PickConfig(..), PickInst(..), Number(..))
import Printing
import Types
import Formula
import Util

import Data.Maybe (fromMaybe)

import Text.PrettyPrint.Leijen.Text




description :: PickInst -> [Either MText Doc]
description PickInst{..} =
              [ Left [ (DE, "Betrachten Sie die folgende Formel in konjunktiver Normalform:")
                     , (UK, "Consider the following formula in conjunctive normal form:")
                     ]
              , Right line
              , Right $ nest 4 $ myText "F = " <+> pretty (cnfs !! (correct - 1))
              , Right line
              , Left [ (DE, "Welche der folgenden Wahrheitstafeln passt zu der Formel? Geben Sie die richtige Tafel durch ihre Nummer an.")
                     , (UK, "Which of these truth tables represents the formula? Specify the correct table by giving its number.")
                     ]
              , Right $ myText (fromMaybe "" addText)
              , Right line
              , Right $ nest 4 $ showIndexedList 120 5 $ map getTable cnfs
              ]




verifyStatic :: PickInst -> Maybe MText
verifyStatic PickInst{..}
    | null cnfs =
        Just [ (DE, "Die Liste der Formeln ist leer.")
             , (UK, "The list of formulae is empty.")
             ]

    | mkCnf [] `elem` cnfs =
        Just [ (DE, "Für mindestens eine Formel kann keine Wahrheitstafel erstellt werden.")
             , (UK, "For at least one given formula there is no corresponding truth table.")
             ]

    | length cnfs < correct || correct <= 0 =
        Just [ (DE, "Der angegebene Index existiert nicht.")
             , (UK, "The given index does not exist.")
             ]

    | otherwise = Nothing




verifyQuiz :: PickConfig -> Maybe MText
verifyQuiz PickConfig{..}


    | amountOfOptions < 2 =
        Just [ (DE, "Es muss mindestens zwei Optionen geben.")
             , (UK, "At least two options need to be given.")
             ]

    | amountOfOptions > 4*2^ length (usedLiterals base) =
        Just [  (DE, "Die Anzahl Optionen übersteigt die Anzahl möglicher, unterschiedlicher Formeln.")
             , (UK, "The amount of options is higher than the amount of possible, distinct formulae.")
             ]

    | otherwise = checkCnfConf cnfConf

  where
    base = baseConf cnfConf







partialGrade :: PickInst -> Cnf -> Maybe MText
partialGrade _ _ = Nothing





completeGrade :: PickInst -> Number -> Maybe MText
completeGrade PickInst{..} sol =
    case sol of Number Nothing -> Just [ (DE, "Es wurde kein Index angegeben.")
                                       , (UK, "You did not give an index.")
                                       ]
                Number (Just index) ->
                  if index == correct then Nothing
                                      else Just [ (DE, "Der gewählte Index ist falsch.")
                                                , (UK, "You submitted the wrong index.")
                                                ]



