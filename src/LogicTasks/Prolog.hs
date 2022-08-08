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


import Control.Monad.Output (
  LangM,
  OutputMonad (..),
  english,
  german,
  translate,
  refuse
  )




description :: OutputMonad m => PrologInst -> LangM m
description PrologInst{..} = do
  paragraph $ translate $ do
    german "Betrachten Sie die zwei folgenden Klauseln:"
    english "Consider the two following clauses:"

    -- PDoc $ nest 4 $ pretty literals1

    -- PDoc $ nest 4 $ pretty literals2

  paragraph $ translate $ do
    german "Resolvieren Sie die Klauseln und geben Sie die Resolvente an."
    english "Resolve the clauses and give the resulting resolvent."

  paragraph $ translate $ do
    german $ "Geben Sie das in dem Resolutionsschritt genutzte Literal und das Ergebnis in der folgenden Tupelform an: (Literal, Resolvente)."
    english "Provide the literal used for the step and the resolvent in the following tuple form: (literal, resolvent)."

  paragraph $ translate $ do
    german "Die leere Klausel kann durch geschweifte Klammern '{}' dargestellt werden."
    english "The empty clause can be denoted by curly braces '{}'."

  paragraph $ text (fromMaybe "" addText)



verifyStatic :: OutputMonad m => PrologInst -> Maybe (LangM m)
verifyStatic PrologInst{..}
    | any isEmptyClause [clause1, clause2] =
        Just $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | not $ resolvable clause1 clause2 =
        Just $ translate $ do
          german "Die Klauseln sind nicht resolvierbar."
          english "The clauses are not resolvable."

    | otherwise = Nothing
  where
    (clause1, clause2, _) = transform (literals1, literals2)




verifyQuiz :: OutputMonad m => PrologConfig -> LangM m
verifyQuiz PrologConfig{..}
    | any (<1) [minClauseLength, maxClauseLength] =
        refuse $ translate $ do
          german "Mindestens eines der 'length'-Parameter ist negativ."
          english "At least one length parameter is negative."

    | minClauseLength > maxClauseLength =
        refuse $ translate $ do
          german "Die untere Grenze der Klausellänge ist höher als die obere."
          english "The minimum clause length is greater than the maximum clause length."

    | length usedPredicates < minClauseLength =
        refuse $ translate $ do
          german "Zu wenige Literale für diese Klausellänge."
          english "There are not enough literals available for this clause length."

    | null usedPredicates =
        refuse $ translate $ do
          german "Es wurden keine Literale angegeben."
          english "You did not specify which literals should be used."

    | otherwise = pure()



start :: (PrologLiteral, PrologClause)
start = (PrologLiteral True " " [], mkPrologClause [])



partialGrade :: OutputMonad m => PrologInst -> (PrologLiteral, PrologClause) -> Maybe (LangM m)
partialGrade PrologInst{..} sol
    | not (fst sol `Set.member` availLits) =
        Just $ translate $ do
          german "Das gewählte Literal kommt in den Klauseln nicht vor."
          english "The chosen literal is not contained in any of the clauses."

    | not (null extra) =
        Just $ paragraph $ do
          translate $ do
            german "In der Resolvente sind unbekannte Literale enthalten. Diese Literale sind falsch: "
            english "The resolvent contains unknown literals. These are incorrect:"
          itemizeM $ map (text . show) extra

    | otherwise = Nothing

  where
     availLits = pliterals literals1 `Set.union` pliterals literals2
     solLits = pliterals $ snd sol
     extra = Set.toList $ solLits `Set.difference` availLits



completeGrade :: OutputMonad m => PrologInst -> (PrologLiteral, PrologClause) -> Maybe (LangM m)
completeGrade PrologInst{..} sol =
    case resolve clause1 clause2 (transSol1) of
        Nothing -> Just $ translate $ do
                     german "Mit diesem Literal kann kein Schritt durchgeführt werden!"
                     english "This literal can not be used for a resolution step!"

        Just solClause -> if (solClause == transSol2)
                            then Nothing
                            else Just $ translate $ do
                                   german "Resolvente ist nicht korrekt."
                                   english "Resolvent is not correct."
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
                                  Nothing -> error "each literal should have a mapping."

    applyPol = map polLookup allPreds
    clause1 = transformProlog pc1 applyPol
    clause2 = transformProlog pc2 applyPol


revertMapping :: [Literal] -> [(PrologLiteral,Literal)] -> [PrologLiteral]
revertMapping ls mapping = map fromJust getPreds
  where
    reverseM = map swap mapping
    getPreds = map (flip lookup reverseM) ls

