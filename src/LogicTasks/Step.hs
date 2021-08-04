{-# language RecordWildCards #-}

module LogicTasks.Step where




import Config (StepConfig(..), StepInst(..))
import Printing
import Types
import Formula
import Util
import Resolution

import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.Leijen.Text




description :: StepInst -> [Either MText Doc]
description StepInst{..} =
              [ Left [  (DE, "Betrachten Sie die zwei folgenden Klauseln:")
                     , (UK, "Consider the two following clauses:")
                     ]
              , Right line
              , Right $ nest 4 $ pretty clause1
              , Right line
              , Right $ nest 4 $ pretty clause2
              , Right line
              , Left [ (DE, "Resolvieren Sie die Klauseln und geben Sie die Resolvente an.")
                     , (UK, "Resolve the clauses and give the resulting resolvent.")
                     ]
              , Left [ (DE, "Geben Sie das in dem Resolutionsschritt genutzte Literal und das Ergebnis in der folgenden Tupelform an:")
                     , (UK, "Provide the literal used for the step and the resolvent in the following tuple form:")
                     ]
              , Left [ (DE, "(Literal, Resolvente als ASCII-basierte Klausel).")
                     , (UK, "(literal, resolvent as ASCII based clause).")
                     ]
              , Left [ (DE, "Beachten Sie dabei für die ASCII-Formel diese Legende:")
                     , (UK, "Consider this key for the ASCII based formula:")
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
              , Right line
              , Right $ myText (fromMaybe "" addText)
              ]





verifyStatic :: StepInst -> Maybe MText
verifyStatic StepInst{..}
    | any isEmptyClause [clause1, clause2] =
        Just [ (DE, "Mindestens eine der Klauseln ist leer.")
             , (UK, "At least one of the clauses is empty.")
             ]
    | not $ resolvable clause1 clause2 =
        Just [ (DE, "Die Klauseln sind nicht resolvierbar.")
             , (UK, "The clauses are not resolvable.")
             ]

    | otherwise = Nothing




verifyQuiz :: StepConfig -> Maybe MText
verifyQuiz StepConfig{..} = checkBaseConf baseConf




partialGrade :: StepInst -> (Literal, Clause) -> Maybe MText
partialGrade StepInst{..} sol
    | not (fst sol `Set.member` availLits) =
        Just [ (DE, "Das gewählte Literal kommt in den Klauseln nicht vor.")
             , (UK, "The chosen literal is not contained in any of the clauses.")
             ]

    | not (null extra) =
        Just [ (DE, "In der Resolvente sind unbekannte Literale enthalten. Diese Literale sind falsch: "
                ++ show extra)
             , (UK, "The resolvent contains unknown literals. These literals are incorrect:"
                ++ show extra)
             ]

    | otherwise = Nothing

  where
     availLits = Set.fromList (literals clause1) `Set.union` Set.fromList (literals clause2)
     solLits = Set.fromList $ literals $ snd sol
     extra = Set.toList (solLits `Set.difference` availLits)





completeGrade :: StepInst -> (Literal, Clause) -> Maybe MText
completeGrade StepInst{..} sol =
    case resolve clause1 clause2 (fst sol) of
        Nothing -> Just [ (DE, "Mit diesem Literal kann kein Schritt durchgeführt werden!")
                        , (UK, "This literal can not be used for a resolution step!")
                        ]
        Just solClause -> if (solClause == snd sol)
                            then Nothing
                            else Just [ (DE, "Resolvente ist nicht korrekt.")
                                      , (UK, "Resolvent is not correct.")
                                      ]









