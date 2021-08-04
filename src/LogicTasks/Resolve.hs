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
              [ Left [  (DE, "Betrachten Sie die folgende Formel in KNF:")
                     , (UK, "Consider the following formula in cnf:")
                     ]
              , Right line
              , Right $ nest 4 $ pretty $ mkCnf clauses
              , Right line
              , Left [ (DE, "Führen Sie das Resolutionsverfahren an dieser Formel durch, um die leere Klausel abzuleiten.")
                     , (UK, "Use the resolution technique on this formula to derive the empty clause.")
                     ]
              , Left [ (DE, "Geben Sie die Lösung als eine Liste von Tripeln an, wobei diese folgendermaßen aufgebaut sind:")
                     , (UK, "Provide the solution as a list of triples with this structure:")
                     ]
              , Left [ (DE, "(Erste Klausel, Zweite Klausel, Resolvente)")
                     , (UK, "(first clause, second clause, resolvent).")
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





verifyStatic :: ResolutionInst -> Maybe MText
verifyStatic ResolutionInst{..}
    | any isEmptyClause clauses =
        Just [ (DE, "Mindestens eine der Klauseln ist leer.")
             , (UK, "At least one of the clauses is empty.")
             ]
    | sat $ mkCnf clauses =
        Just [ (DE, "Die Formel ist erfüllbar.")
             , (UK, "This formula is satisfiable.")
             ]

    | otherwise = Nothing




verifyQuiz :: ResolutionConfig -> Maybe MText
verifyQuiz ResolutionConfig{..}
    | minSteps < 1 =
        Just [ (DE, "Die Mindestschritte müssen größer als 0 sein.")
             , (UK, "The minimal amount of steps must be greater than 0.")
             ]

    | maxClauseLength baseConf == 1 && minSteps > 1 =
        Just [ (DE, "Mit Klauseln der Länge 1 kann nicht mehr als ein Schritt durchgeführt werden.")
             , (UK, "More than one step using only length 1 clauses is not possible.")
             ]

    | minSteps > 2 * length (usedLiterals baseConf) =
        Just [ (DE, "Diese minimale Schrittzahl kann mit den gegebenen Literalen nicht durchgeführt werden.")
             , (UK, "This amount of steps is impossible with the given amount of literals.")
             ]

    | otherwise = checkBaseConf baseConf





partialGrade :: ResolutionInst -> [(Clause,Clause,Clause)] -> Maybe MText
partialGrade ResolutionInst{..} sol
    | not (null wrongLitsSteps) =
        Just [ (DE, "Mindestens ein Schritt beinhaltet Literale, die in der Formel nicht vorkommen. "
                ++ show wrongLitsSteps)
             , (UK, "At least one step contains literals not found in the original formula. "
                ++ show wrongLitsSteps)
             ]

    | not (null noResolveSteps) =
        Just [ (DE, "Mindestens ein Schritt ist kein gültiger Resolutionsschritt. "
                ++ show noResolveSteps)
             , (UK, "At least one step is not a valid resolution step. "
                ++ show noResolveSteps)
             ]

    | not (isEmptyClause $ thrd3 $ last sol) =
        Just [  (DE, "Im letzten Schritt muss die leere Klausel abgeleitet werden.")
             , (UK, "The last step must derive the empty clause.")
             ]

    | otherwise = Nothing

  where
    availLits = Set.unions (map (Set.fromList . literals) clauses)
    stepLits (c1,c2,r) = Set.toList $ Set.unions $ map (Set.fromList . literals) [c1,c2,r]
    wrongLitsSteps = filter (not . all (`Set.member` availLits) . stepLits) sol
    noResolveSteps = filter (\(c1,c2,r) -> maybe True (\x -> fromJust (resolve c1 c2 x) /= r) (resolvableWith c1 c2)) sol






completeGrade :: ResolutionInst -> [(Clause,Clause,Clause)] -> Maybe MText
completeGrade ResolutionInst{..} sol =
    case applySteps clauses sol of
        Nothing -> Just [  (DE, "In mindestens einem Schritt werden Klauseln resolviert, die nicht in der Formel sind oder noch nicht abgeleitet wurden.")
                        , (UK, "In at least one step clauses are used, that are not part of the original formula and are not derived from previous steps.")
                        ]
        Just solClauses -> if (any isEmptyClause solClauses)
                            then Nothing
                            else Just [  (DE, "Die Leere Klausel wurde nicht korrekt abgeleitet.")
                                      , (UK, "The Empty clause was not derived correctly.")
                                      ]









