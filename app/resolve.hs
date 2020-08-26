{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Main where



import Control.Exception(try,SomeException)
import Data.Set (insert,fromList,empty)
import Test.QuickCheck (suchThat,generate,elements)
import Formula (Clause(..),Literal(..),opposite)
import Resolution (resolve,applySteps,genRes,showResClauses)
import TaskUtility
import Types




resolutionExercise :: ResolutionConfig -> IO()
resolutionExercise = ensureChecksAndExecute checkResolutionConfig executeResolutionExercise

  where executeResolutionExercise resolutionConfig = do
          (desc,numberedClauses) <- genResolutionExercise resolutionConfig
          putStrLn desc
          evaluateResolve numberedClauses



genResolutionExercise :: ResolutionConfig -> IO (String,[(Int,Clause)])
genResolutionExercise ResolutionConfig { minClauseLength, maxClauseLength, steps, usedLiterals} = do
 clauses <- generate (genRes (minClauseLength,maxClauseLength) steps usedLiterals)
 let numberedClauses = zip [1..] clauses
 let desc = exerciseDescResolve numberedClauses
 return (desc,numberedClauses)



exerciseDescResolve ::  [(Int,Clause)] -> String
exerciseDescResolve clauses = "Fuehren Sie das Resolutionsverfahren mit der folgenden Klauselmenge durch.\n" ++
 showResClauses clauses ++ "\n" ++
 "Geben Sie die Loesung als eine Liste von Tripeln an, wobei die Tripel nach dem Muster (Erster Index, Zweiter Index, ausgewähltes Literal) aufgebaut sind.\n" ++
 "Neu resolvierte Klauseln erhalten dabei fortlaufend den naechst hoeheren Index.\n"



evaluateResolve :: [(Int,Clause)] -> IO()
evaluateResolve clauses = do
 solution <- try readLn :: IO (Either SomeException [(Int,Int,Literal)])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s -> case applySteps clauses s of Just result -> if Clause empty `elem` map snd result then putStrLn "Richtige Lösung"
                                                                                                            else putStrLn "Falsche Lösung"
                                                          _           -> error "Falsches Ergebnis, die leere Klausel wurde nicht resolviert."



main :: IO()
main = resolutionExercise defaultResolutionConfig