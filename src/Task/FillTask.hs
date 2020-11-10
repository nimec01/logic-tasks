{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}

module Task.FillTask
      ( genFillExercise
      , exerciseDescFill
      , evaluateFill
      , solver
      ) where



import Control.Exception (try,SomeException)
import Test.QuickCheck (generate)
import Formula (Cnf,getLiterals,genCnf, partEvalCnf)
import Table (Table,getTable,fillGaps,genGapTable,countDiffEntries,readEntries, possibleAllocations)
import Types (FillConfig(..),CnfConfig(..),ClauseConfig(..))
import Task.Utility (withRatio,noSequences)


solver :: Table -> Cnf -> Int
solver gapTable cnf = sum $ map snd solveForAllocs
  where
    allocs = possibleAllocations (getLiterals cnf)
    zipped = zip allocs (readEntries gapTable)
    blankOnly = map fst (filter (\(_,y) -> y == Nothing) zipped)
    solveForAllocs = map (partEvalCnf cnf) blankOnly




genFillExercise :: FillConfig -> IO (String,(Cnf,Table,Table))
genFillExercise
    FillConfig {cnfConfig = CnfConfig {clauseConf = ClauseConfig {..},..},..}
  = do
    cnf <- generate cnfInRange
    let table = getTable cnf
    gapTable <- generate (genGapTable table percentageOfGaps)
    let desc = exerciseDescFill cnf gapTable
    pure (desc,(cnf,table,gapTable))
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)
                     usedLiterals
    cnfInRange = case percentTrueEntries of
        Just range -> cnfWithRatio range
        Nothing    -> getCnf

    cnfWithRatio ratio = do
        cnf <- getCnf
        let entries = readEntries (getTable cnf)
        if withRatio ratio cnf -- && noSequences 4 entries
          then pure cnf
          else cnfWithRatio ratio




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
    case solution of
        Left _  -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
        Right s -> let
            filledTable = fillGaps s gapTable
            diffCount = countDiffEntries filledTable table
            numerus = if diffCount == 1 then " falschen Eintrag" else " falsche Eintraege"
            errorDisplay = "Die Lösung enthält " ++ show diffCount ++ numerus
          in
            putStr (if filledTable == table then "Richtige Lösung" else errorDisplay)