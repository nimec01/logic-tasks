{-# language RecordWildCards #-}

module LogicTasks.Prolog where



import Config (PrologConfig(..), PrologInst(..))
import Printing
import Types
import Formula
import Resolution

import qualified Data.Set as Set
import Data.Maybe (fromMaybe, fromJust)
import Data.Tuple(swap)

import Text.PrettyPrint.Leijen.Text



description :: PrologInst -> [ProxyDoc]
description PrologInst{..} =
              [ PMult ("Betrachten Sie die zwei folgenden Klauseln:"
                     ,"Consider the two following clauses:"
                     )
              , PDoc line
              , PDoc $ nest 4 $ pretty literals1
              , PDoc line
              , PDoc $ nest 4 $ pretty literals2
              , PDoc line
              , PMult ("Resolvieren Sie die Klauseln und geben Sie die Resolvente an."
                     ,"Resolve the clauses and give the resulting resolvent."
                     )
              , PMult ("Geben Sie den in dem Resolutionsschritt genutzten Term und das Ergebnis in der folgenden Tupelform an: "
                        ++ "(Literal, Term)."
                     ,"Provide the term used for the step and the resolvent in the following tuple form: "
                        ++ "(literal, term)."
                     )
              , PDoc line
              , PDoc $ myText (fromMaybe "" addText)
              ]



verifyStatic :: PrologInst -> Maybe ProxyDoc
verifyStatic PrologInst{..}
    | any isEmptyClause [clause1, clause2] =
        Just $ PMult ("Mindestens eine der Klauseln ist leer."
                     ,"At least one of the clauses is empty."
                     )
    | not $ resolvable clause1 clause2 =
        Just $ PMult ("Die Klauseln sind nicht resolvierbar."
                     ,"The clauses are not resolvable."
                     )

    | otherwise = Nothing
  where
    (clause1, clause2, _) = transform (literals1, literals2)




verifyQuiz :: PrologConfig -> Maybe ProxyDoc
verifyQuiz PrologConfig{..}
    | any (<1) [minClauseLength, maxClauseLength] =
        Just $ PMult ("Mindestens eines der 'length'-Parameter ist negativ."
                     ,"At least one length parameter is negative."
                     )

    | minClauseLength > maxClauseLength =
        Just $ PMult ("Die untere Grenze der Klausellänge ist höher als die obere."
                     ,"The minimum clause length is greater than the maximum clause length."
                     )

    | length usedPredicates < minClauseLength =
        Just $ PMult ("Zu wenige Literale für diese Klausellänge."
                     ,"There are not enough literals available for this clause length."
                     )

    | null usedPredicates =
        Just $ PMult ("Es wurden keine Terme angegeben."
                     ,"You did not specify which terms should be used."
                     )

    | otherwise = Nothing



start :: (PrologLiteral, PrologClause)
start = (PrologLiteral True " " [], mkPrologClause [])



partialGrade :: PrologInst -> (PrologLiteral, PrologClause) -> Maybe ProxyDoc
partialGrade PrologInst{..} sol
    | not (transSol1 `Set.member` availLits) =
        Just $ PMult ("Der gewählte Term kommt in den Klauseln nicht vor."
                     ,"The chosen term is not contained in any of the clauses."
                     )

    | not (null extra) =
        Just $ Composite [ PMult ("In der Resolvente sind unbekannte Terme enthalten. Diese Terme sind falsch: "
                                 ,"The resolvent contains unknown terms. These terms are incorrect:"
                                 )
                         , PDoc $ pretty extra
                         ]

    | otherwise = Nothing

  where
     (clause1, clause2, mapping) = transform (literals1, literals2)
     transSol1 = fromJust $ lookup (fst sol) mapping
     transSol2 = transformProlog (snd sol) mapping
     availLits = Set.fromList (literals clause1) `Set.union` Set.fromList (literals clause2)
     solLits = Set.fromList $ literals $ transSol2
     extra = revertMapping (Set.toList (solLits `Set.difference` availLits)) mapping



completeGrade :: PrologInst -> (PrologLiteral, PrologClause) -> Maybe ProxyDoc
completeGrade PrologInst{..} sol =
    case resolve clause1 clause2 (transSol1) of
        Nothing -> Just $ PMult ("Mit diesem Literal kann kein Schritt durchgeführt werden!"
                                ,"This literal can not be used for a resolution step!"
                                )
        Just solClause -> if (solClause == transSol2)
                            then Nothing
                            else Just $ PMult ("Resolvente ist nicht korrekt."
                                              ,"Resolvent is not correct."
                                              )
  where
    (clause1, clause2, mapping) = transform (literals1, literals2)
    transSol1 = fromJust $ lookup (fst sol) mapping
    transSol2 = transformProlog (snd sol) mapping





transform :: (PrologClause,PrologClause) -> (Clause,Clause,[(PrologLiteral,Literal)])
transform (pc1,pc2) = (clause1, clause2, applyPol)
  where
    allPreds = Set.toList (Set.union (pliterals pc1) (pliterals pc2))
    noDups = map (\(PrologLiteral _ n f) -> PrologLiteral True n f) allPreds
    mapping = zip noDups ['A'..'Z']

    polLookup p = (p,lit)
      where lit = case lookup p mapping of
                    Just l1  -> Literal l1
                    Nothing  -> case lookup (flipPol p) mapping of
                                  Just l2 -> Not l2
                                  Nothing -> error "each term should have a mapping."

    applyPol = map polLookup allPreds
    clause1 = transformProlog pc1 applyPol
    clause2 = transformProlog pc2 applyPol


revertMapping :: [Literal] -> [(PrologLiteral,Literal)] -> [PrologLiteral]
revertMapping ls mapping = map fromJust getPreds
  where
    reverseM = map swap mapping
    getPreds = map (flip lookup reverseM) ls

