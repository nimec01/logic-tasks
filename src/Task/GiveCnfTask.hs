{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}

module Task.GiveCnfTask
      ( genGiveCnfExercise
      , exerciseDescCnf
      , evaluateCnf
      ) where



import Control.Exception (try,SomeException)
import Data.Set (fromList)
import Test.QuickCheck (generate)
import Formula (Literal,Clause(..),Cnf(..),genCnf)
import Task.Utility
import Types
import Table(Table,getTable)



genGiveCnfExercise :: GiveCnfConfig -> IO (String,Table)
genGiveCnfExercise
    GiveCnfConfig {cnfConfig = CnfConfig {clauseConf = ClauseConfig {..}, ..}, ..}
  = do
    cnf <- generate cnfInRange
    let
      table = getTable cnf
      desc = exerciseDescCnf table
    return (desc,table)
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)
                     usedLiterals
    cnfInRange =
        case percentTrueEntries of Just range -> cnfWithRatio range
                                   Nothing    -> getCnf

    cnfWithRatio ratio = do
        cnf <- getCnf
        if withRatio ratio cnf
          then pure cnf
          else cnfWithRatio ratio


exerciseDescCnf :: Table -> String
exerciseDescCnf table =
    "Betrachten Sie die folgende Wahrheitstafel: \n\n" ++ show table ++
    "\n\nGeben Sie eine zu der Tafel passende Formel in konjunktiver Normalform an. Verwenden Sie dazu Max-Terme.\n" ++
    "\nReichen Sie ihre Loesung in Form einer Liste von Listen von Literalen ein, wobei jede innere Liste einer Klausel der KNF entspricht."




evaluateCnf :: Table -> IO ()
evaluateCnf table = do
    solution <- try readLn :: IO (Either SomeException [[Literal]])
    case solution of
        Left _  -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
        Right s -> let
            answer = getTable (Cnf (fromList (map (Clause . fromList) s)))
          in
            putStr (if table == answer then "Richtige Lösung" else "Falsche Lösung")