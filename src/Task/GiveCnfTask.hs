{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields, RecordWildCards #-}

module Task.GiveCnfTask
      ( genGiveCnfExercise
      , exerciseDescCnf
      , evaluateCnf
      ) where



import Control.Exception (try,SomeException)
import Data.Set (fromList)
import Test.QuickCheck (generate,chooseInt,suchThat)
import Formula (Literal,Clause(..),Cnf(..),genCnf)
import Task.Utility
import Types
import Table(Table,getTable)



genGiveCnfExercise :: GiveCnfConfig -> IO (String,Table)
genGiveCnfExercise GiveCnfConfig {cnfConfig = CnfConfig {clauseConf = ClauseConfig {..}, ..}, ..} = do
 cnf <- generate (case percentTrueEntries of Just (lower,upper) -> do ratio <- chooseInt (lower,upper)
                                                                      suchThat getCnf (withRatio ratio)
                                             Nothing            -> getCnf)
 let table = getTable cnf
 let desc = exerciseDescCnf table
 return (desc,table)
  where getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals



exerciseDescCnf :: Table -> String
exerciseDescCnf table =
 "Betrachten Sie die folgende Wahrheitstafel: \n\n" ++ show table ++
 "\n\nGeben Sie eine zu der Tafel passende Formel in konjunktiver Normalform an. Verwenden Sie dazu Max-Terme.\n" ++
 "\nGeben Sie eine Liste von Listen von Literalen an, wobei jede innere Liste einer Klausel der KNF entspricht."




evaluateCnf :: Table -> IO ()
evaluateCnf table = do
  solution <- try readLn :: IO (Either SomeException [[Literal]])
  case solution of Left _ -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                   Right s ->   putStr (if table == getTable (Cnf (fromList (map (Clause . fromList) s))) then "Richtige Lösung" else "Falsche Lösung")