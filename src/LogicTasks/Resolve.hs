{-# language RecordWildCards #-}

module LogicTasks.Resolve where




import Config (ResolutionConfig(..), ResolutionInst(..), BaseConfig(..))
import Printing
import Types
import Formula
import Util
import Resolution

import qualified Data.Set as Set
import Data.Maybe (fromMaybe, fromJust)

import Text.PrettyPrint.Leijen.Text



fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b


thrd3 :: (a,b,c) -> c
thrd3 (_,_,c) = c



description :: ResolutionInst -> [Either MText Doc]
description ResolutionInst{..} =
              [ Left ("Betrachten Sie die folgende Formel in KNF:"
                     ,"Consider the following formula in cnf:"
                     )
              , Right line
              , Right $ nest 4 $ pretty $ mkCnf clauses
              , Right line
              , Left ("Führen Sie das Resolutionsverfahren an dieser Formel durch, um die leere Klausel abzuleiten."
                     ,"Use the resolution technique on this formula to derive the empty clause."
                     )
              , Left ("Geben Sie die Lösung als eine Liste von Tripeln an, wobei diese folgendermaßen aufgebaut sind:"
                     ,"Provide the solution as a list of triples with this structure:"
                     )
              , Left ("(Erste Klausel, Zweite Klausel, Resolvente)"
                     ,"(first clause, second clause, resolvent)."
                     )
              , Left ("Beachten Sie dabei für die ASCII-Formel diese Legende:"
                     ,"Consider this key for the ASCII based formula:"
                     )
              , Right line
              , Left ("Negation"
                     ,"negation"
                     )
              , Right $ myText ": ~"
              , Left ("oder"
                     ,"or"
                     )
              , Right $ myText ": \\/"
              , Right line
              , Right $ myText (fromMaybe "" addText)
              ]



verifyStatic :: ResolutionInst -> Maybe MText
verifyStatic ResolutionInst{..}
    | any isEmptyClause clauses =
        Just ("Mindestens eine der Klauseln ist leer."
             ,"At least one of the clauses is empty."
             )
    | sat $ mkCnf clauses =
        Just ("Die Formel ist erfüllbar."
             ,"This formula is satisfiable."
             )

    | otherwise = Nothing



verifyQuiz :: ResolutionConfig -> Maybe MText
verifyQuiz ResolutionConfig{..}
    | minSteps < 1 =
        Just ("Die Mindestschritte müssen größer als 0 sein."
             ,"The minimal amount of steps must be greater than 0."
             )

    | maxClauseLength baseConf == 1 && minSteps > 1 =
        Just ("Mit Klauseln der Länge 1 kann nicht mehr als ein Schritt durchgeführt werden."
             ,"More than one step using only length 1 clauses is not possible."
             )

    | minSteps > 2 * length (usedLiterals baseConf) =
        Just ("Diese minimale Schrittzahl kann mit den gegebenen Literalen nicht durchgeführt werden."
             ,"This amount of steps is impossible with the given amount of literals."
             )

    | otherwise = checkBaseConf baseConf



start :: [ResStep]
start = []



partialGrade :: ResolutionInst -> [ResStep] -> Maybe MText
partialGrade ResolutionInst{..} sol
    | not (null wrongLitsSteps) =
        Just ("Mindestens ein Schritt beinhaltet Literale, die in der Formel nicht vorkommen. "
                ++ show wrongLitsSteps
             ,"At least one step contains literals not found in the original formula. "
                ++ show wrongLitsSteps
             )

    | not (null noResolveSteps) =
        Just ("Mindestens ein Schritt ist kein gültiger Resolutionsschritt. "
                ++ show noResolveSteps
             ,"At least one step is not a valid resolution step. "
                ++ show noResolveSteps
             )

    | not (isEmptyClause $ thrd3 $ last steps) =
        Just ("Im letzten Schritt muss die leere Klausel abgeleitet werden."
             ,"The last step must derive the empty clause."
             )

    | otherwise = Nothing

  where
    steps = map trip sol
    availLits = Set.unions (map (Set.fromList . literals) clauses)
    stepLits (c1,c2,r) = Set.toList $ Set.unions $ map (Set.fromList . literals) [c1,c2,r]
    wrongLitsSteps = filter (not . all (`Set.member` availLits) . stepLits) steps
    noResolveSteps = filter (\(c1,c2,r) -> maybe True (\x -> fromJust (resolve c1 c2 x) /= r) (resolvableWith c1 c2)) steps



completeGrade :: ResolutionInst -> [ResStep] -> Maybe MText
completeGrade ResolutionInst{..} sol =
    case applySteps clauses (map trip sol) of
        Nothing -> Just ("In mindestens einem Schritt werden Klauseln resolviert, die nicht in der Formel sind oder noch nicht abgeleitet wurden."
                        ,"In at least one step clauses are used, that are not part of the original formula and are not derived from previous steps."
                        )
        Just solClauses -> if (any isEmptyClause solClauses)
                            then Nothing
                            else Just ("Die Leere Klausel wurde nicht korrekt abgeleitet."
                                      ,"The Empty clause was not derived correctly."
                                      )
