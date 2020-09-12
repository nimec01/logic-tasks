{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}

module Task.FillTask
      ( genFillExercise
      , exerciseDescFill
      , evaluateFill
      ) where



import Control.Exception (try,SomeException)
import Test.QuickCheck (generate,suchThat,chooseInt)
import Formula (Cnf,genCnf)
import Table (Table,getTable,fillGaps,genGapTable,countDiffEntries)
import Types (FillConfig(..),CnfConfig(..),ClauseConfig(..))
import Task.Utility (withRatio)



genFillExercise :: FillConfig -> IO (String,(Table,Table))
genFillExercise
    FillConfig {cnfConfig = CnfConfig {clauseConf = ClauseConfig {..},..},..}
  = do
    cnf <- generate cnfInRange
    let table = getTable cnf
    gapTable <- generate (genGapTable table amountOfGaps)
    let desc = exerciseDescFill cnf gapTable
    pure (desc,(table,gapTable))
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)
                     usedLiterals
    cnfInRange = case percentTrueEntries of Just range -> do ratio <- chooseInt range
                                                             suchThat getCnf (withRatio ratio)
                                            Nothing    -> getCnf



exerciseDescFill :: Cnf -> Table -> String
exerciseDescFill cnf table =
    "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n\n" ++
    show cnf ++
    "\nFuellen Sie in der zugehoerigen Wahrheitstafel alle Luecken mit einem passenden Wahrheitswert (True oder False) \n\n" ++
    show table ++
    "\nGeben Sie als Loesung eine Liste der fehlenden Werte an, wobei das erste Element der Liste der ersten Zeile entspricht, das zweite ELement der zweiten Zeile, etc."



evaluateFill :: Table -> Table -> IO ()
evaluateFill table gapTable = do
    solution <- try readLn :: IO (Either SomeException [Bool])
    case solution of Left _  -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                     Right s -> let
                                  filledTable = fillGaps s gapTable
                                  diffCount = countDiffEntries filledTable table
                                  errorDisplay = "Die Lösung enthält " ++ show diffCount ++ if diffCount == 1 then " falschen Eintrag" else " falsche Eintraege"
                                in
                                  putStr (if filledTable == table then "Richtige Lösung" else errorDisplay)