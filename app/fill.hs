{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields, RecordWildCards #-}
module Main where

import Control.Exception (try,SomeException)
import Test.QuickCheck (generate,suchThat,chooseInt)
import Formula (CNF,genCNF)
import Table (Table,getTable,fillGaps,genGapTable,countDiffEntries)
import TaskUtility
import Types



fillExercise :: FillConfig -> IO()
fillExercise = ensureChecksAndExecute checkFillConfig executeFillExercise

  where executeFillExercise fillConfig = do
         (desc,(table,gapTable)) <- genFillExercise fillConfig
         putStrLn desc         
         evaluateFill table gapTable
          
 
 

genFillExercise :: FillConfig -> IO (String,(Table,Table))
genFillExercise FillConfig
  { cnfConfig = CnfConfig {clauseConf = ClauseConfig {..}, ..}, ..} = do 
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
                  Right s ->   let filledTable = fillGaps s gapTable
                                   diffCount = countDiffEntries filledTable table  in 
                    putStr (if filledTable == table then "Richtige Lösung" else "Die Lösung enthält " ++ show diffCount ++ if diffCount == 1 then " falschen Eintrag" else " falsche Eintraege" )


main :: IO()
main = fillExercise defaultFillConfig