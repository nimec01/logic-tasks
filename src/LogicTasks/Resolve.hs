{-# language RecordWildCards #-}

module LogicTasks.Resolve where




import Config (ResolutionConfig(..), ResolutionInst(..), BaseConfig(..))
import Printing
import Types
import Formula
import Util
import Resolution

import qualified Data.Set as Set
import Data.List (sort)
import Data.Maybe (fromMaybe, fromJust, isJust)

import Text.PrettyPrint.Leijen.Text



fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b


thrd3 :: (a,b,c) -> c
thrd3 (_,_,c) = c



description :: ResolutionInst -> [ProxyDoc]
description ResolutionInst{..} =
              [ PMult ("Betrachten Sie die folgende Formel in KNF:"
                      ,"Consider the following formula in cnf:"
                      )
              , PDoc line
              , PDoc $ nest 4 $ pretty $ mkCnf clauses
              , PDoc line
              , PMult ("Führen Sie das Resolutionsverfahren an dieser Formel durch, um die leere Klausel abzuleiten."
                      ,"Use the resolution technique on this formula to derive the empty clause."
                      )
              , PMult ("Geben Sie die Lösung als eine Liste von Tripeln an, wobei diese folgendermaßen aufgebaut sind:"
                      ,"Provide the solution as a list of triples with this structure:"
                      )
              , PMult ("(Erste Klausel, Zweite Klausel, Resolvente)"
                      ,"(first clause, second clause, resolvent)."
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
              , Composite [ PMult ("leere Klausel"
                                  ,"empty clause"
                                  )
                                  , PDoc $ myText ": { }"
                                  ]
              , PDoc line
              , PMult ("Optional können Sie Klauseln auch durch Nummern substituieren."
                      ,"You can optionally substitute clauses with numbers"
                      )
              , PDoc line
              , PMult ("Klauseln aus der Formel sind bereits ihrer Reihenfolge nach nummeriert. (erste Klausel = 1, zweite Klausel = 2, ...)"
                      ,"Clauses in the starting formula are already numbered by their order. (first clause = 1, second clause = 2, ...)"
                      )
              , PDoc line
              , PMult ("neu resolvierte Klauseln können mit einer Nummer versehen werden, indem Sie '= NUMMER' an diese anfügen."
                      ,"Newly resolved clauses can be associated with a number by attaching '= NUMBER' behind them."
                      )
              , PDoc line
              , Composite [ PMult ("Ein Lösungsversuch könnte beispielsweise so aussehen: "
                                  , "A valid solution could look like this: ")
                          , PDoc $ myText "[(1, 2, {A, ~B} = 5), (4, 5, { })]"
                          ]
              , PDoc line
              , PDoc $ myText (fromMaybe "" addText)
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
    | isJust checkMapping  = checkMapping

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

    | checkEmptyClause =
        Just ("Im letzten Schritt muss die leere Klausel abgeleitet werden."
             ,"The last step must derive the empty clause."
             )

    | otherwise = Nothing

  where
    checkMapping = correctMapping sol $ baseMapping clauses
    steps =  replaceAll sol $ baseMapping clauses
    checkEmptyClause = null steps || not (isEmptyClause $ thrd3 $ last steps)
    availLits = Set.unions (map (Set.fromList . literals) clauses)
    stepLits (c1,c2,r) = Set.toList $ Set.unions $ map (Set.fromList . literals) [c1,c2,r]
    wrongLitsSteps = filter (not . all (`Set.member` availLits) . stepLits) steps
    noResolveSteps = filter (\(c1,c2,r) -> maybe True (\x -> fromJust (resolve c1 c2 x) /= r) (resolvableWith c1 c2)) steps



completeGrade :: ResolutionInst -> [ResStep] -> Maybe MText
completeGrade ResolutionInst{..} sol =
    case applySteps clauses steps of
        Nothing -> Just ("In mindestens einem Schritt werden Klauseln resolviert, die nicht in der Formel sind oder noch nicht abgeleitet wurden."
                        ,"In at least one step clauses are used, that are not part of the original formula and are not derived from previous steps."
                        )
        Just solClauses -> if (any isEmptyClause solClauses)
                            then Nothing
                            else Just ("Die Leere Klausel wurde nicht korrekt abgeleitet."
                                      ,"The Empty clause was not derived correctly."
                                      )

      where
        steps = replaceAll sol $ baseMapping clauses



baseMapping :: [Clause] -> [(Int,Clause)]
baseMapping xs = zip [1..] $ sort xs


correctMapping :: [ResStep] -> [(Int,Clause)] -> Maybe MText
correctMapping [] _ = Nothing
correctMapping (Res (c1,c2,(c3,i)): rest) mapping
    | checkIndices = Just ("Mindestens ein Schritt verwendet einen nicht vergebenen Index. "
                          ,"At least one step is using an unknown index."
                          )
    | alreadyUsed i = Just ("Mindestens ein Schritt vergibt einen Index, welcher bereits verwendet wird. "
                           ,"At least one step assigns an index, which is already in use. "
                           )
    | otherwise = correctMapping rest newMapping


  where
    newMapping = case i of Nothing      -> mapping
                           (Just index) -> (index,c3) : mapping


    unknown (Left _) = False
    unknown (Right n) = n `notElem` (map fst mapping)

    checkIndices = unknown c1 || unknown c2

    alreadyUsed Nothing = False
    alreadyUsed (Just n) = n `elem` (map fst mapping)



replaceAll :: [ResStep] -> [(Int,Clause)] -> [(Clause,Clause,Clause)]
replaceAll [] _ = []
replaceAll (Res (c1,c2,(c3,i)) : rest) mapping = (replaceNum c1, replaceNum c2, c3) : replaceAll rest newMapping
  where
    newMapping = case i of Nothing      -> mapping
                           (Just index) -> (index,c3) : mapping

    replaceNum (Left c) = c
    replaceNum (Right n) = case lookup n mapping of Nothing  -> error "no mapping"
                                                    (Just c) -> c
