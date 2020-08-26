{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Main where

import Control.Exception (try,SomeException)
import Data.List(delete)
import Data.Set (empty,toList,fromList,insert)
import Test.QuickCheck (generate,vectorOf,elements, suchThat, chooseInt)
import Formula (Literal(..),CNF(..),Clause(..),genClause,genCNF,opposite)
import Table (Table,getTable,evalSolution,genGapTable,genWrongTable,readEntries)
import TaskUtility
import Types
import Resolution (genRes,resolve,applySteps,showResClauses)



fillExercise :: FillConfig -> IO()
fillExercise = ensureChecksAndExecute checkFillConfig executeFillExercise

  where executeFillExercise fillConfig = do
         (desc,(table,gapTable)) <- genFillExercise fillConfig
         putStrLn desc         
         evaluateFill table gapTable
          
 
 

genFillExercise :: FillConfig -> IO (String,(Table,Table))
genFillExercise FillConfig
  { minClauseAmount
  , maxClauseAmount
  , minClauseLength
  , maxClauseLength
  , usedLiterals
  , amountOfGaps
  , percentTrueEntries
  }  = do 
 cnf <- generate (case percentTrueEntries of Just (lower,upper) -> do ratio <- chooseInt (lower,upper)
                                                                      suchThat getCNF (withRatio ratio)
                                             Nothing            -> getCNF)
 let table = getTable cnf
 gapTable <- generate (genGapTable table amountOfGaps)
 let desc = exerciseDescFill cnf gapTable  
 return (desc,(table,gapTable))
  where getCNF = genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals      



exerciseDescFill :: CNF -> Table -> String
exerciseDescFill cnf table = 
 "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n\n" ++
 show cnf ++
 "\nFuellen Sie in der zugehoerigen Wahrheitstafel alle Luecken mit einem passenden Wahrheitswert (True oder False) \n\n" ++
 show table ++
 "\nGeben Sie als Loesung eine Liste der fehlenden Werte an, wobei das erste Element der Liste der ersten Zeile entspricht, das zweite ELement der zweiten Zeile, etc."
 
 
 
evaluateFill :: Table -> Table -> IO ()
evaluateFill table gapTable = do
 solution <- try readLn :: IO (Either SomeException [Bool])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s ->   putStr (if evalSolution s table gapTable then "Richtige Lösung" else "Falsche Lösung")



main :: IO()
main = fillExercise defaultFillConfig