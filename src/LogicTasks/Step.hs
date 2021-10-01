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



description :: StepInst -> [ProxyDoc]
description StepInst{..} =
              [ PMult ("Betrachten Sie die zwei folgenden Klauseln:"
                     ,"Consider the two following clauses:"
                     )
              , PDoc line
              , PDoc $ nest 4 $ pretty clause1
              , PDoc line
              , PDoc $ nest 4 $ pretty clause2
              , PDoc line
              , PMult ("Resolvieren Sie die Klauseln und geben Sie die Resolvente an."
                     ,"Resolve the clauses and give the resulting resolvent."
                     )
              , PMult ("Geben Sie das in dem Resolutionsschritt genutzte Literal und das Ergebnis in der folgenden Tupelform an:"
                     ,"Provide the literal used for the step and the resolvent in the following tuple form:"
                     )
              , PMult ("(Literal, Resolvente als ASCII-basierte Klausel)."
                     ,"(literal, resolvent as ASCII based clause)."
                     )
              , PMult ("Beachten Sie dabei für die ASCII-Formel diese Legende:"
                     ,"Consider this key for the ASCII based formula:"
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
              , PDoc line
              , PDoc $ myText (fromMaybe "" addText)
              ]



verifyStatic :: StepInst -> Maybe MText
verifyStatic StepInst{..}
    | any isEmptyClause [clause1, clause2] =
        Just ("Mindestens eine der Klauseln ist leer."
             ,"At least one of the clauses is empty."
             )
    | not $ resolvable clause1 clause2 =
        Just ("Die Klauseln sind nicht resolvierbar."
             ,"The clauses are not resolvable."
             )

    | otherwise = Nothing



verifyQuiz :: StepConfig -> Maybe MText
verifyQuiz StepConfig{..} = checkBaseConf baseConf



start :: (Literal, Clause)
start = (Literal ' ', mkClause [])



partialGrade :: StepInst -> (Literal, Clause) -> Maybe MText
partialGrade StepInst{..} sol
    | not (fst sol `Set.member` availLits) =
        Just ("Das gewählte Literal kommt in den Klauseln nicht vor."
             ,"The chosen literal is not contained in any of the clauses."
             )

    | not (null extra) =
        Just ("In der Resolvente sind unbekannte Literale enthalten. Diese Literale sind falsch: "
                ++ show extra
             ,"The resolvent contains unknown literals. These literals are incorrect:"
                ++ show extra
             )

    | otherwise = Nothing

  where
     availLits = Set.fromList (literals clause1) `Set.union` Set.fromList (literals clause2)
     solLits = Set.fromList $ literals $ snd sol
     extra = Set.toList (solLits `Set.difference` availLits)



completeGrade :: StepInst -> (Literal, Clause) -> Maybe MText
completeGrade StepInst{..} sol =
    case resolve clause1 clause2 (fst sol) of
        Nothing -> Just ("Mit diesem Literal kann kein Schritt durchgeführt werden!"
                        ,"This literal can not be used for a resolution step!"
                        )
        Just solClause -> if (solClause == snd sol)
                            then Nothing
                            else Just ("Resolvente ist nicht korrekt."
                                      ,"Resolvent is not correct."
                                      )
