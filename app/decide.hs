{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields, RecordWildCards #-}
module Main where


import Control.Exception (try,SomeException)
import Data.Set (fromList,toList)
import Test.QuickCheck (generate,elements)
import Formula (CNF,genCNF)
import Table (Table,getTable,genWrongTable)
import TaskUtility
import Types



decideExercise :: DecideConfig -> IO()
decideExercise = ensureChecksAndExecute checkDecideConfig executeDecideExercise

  where executeDecideExercise decideConfig = do
          (desc,(rightTable,displayTable,indices)) <- genDecideExercise decideConfig
          putStrLn desc
          if mistakes
           then evaluateDecide2 indices
           else evaluateDecide (displayTable == rightTable)
            where mistakes = findMistakes decideConfig



genDecideExercise :: DecideConfig -> IO (String,(Table,Table,[Int]))
genDecideExercise DecideConfig {cnfConfig = CnfConfig {clauseConf = ClauseConfig{..}, ..}, ..} = do
 cnf <- generate (genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals)
 let rightTable = getTable cnf
 (indices,wrongTable) <- generate $ genWrongTable rightTable amountOfChanges
 displayTable <- if findMistakes then return wrongTable else generate $ elements [rightTable,wrongTable]
 let desc = exerciseDescDecide cnf displayTable findMistakes
 return (desc,(rightTable,displayTable,indices))



exerciseDescDecide :: CNF -> Table -> Bool -> String
exerciseDescDecide cnf table mode =
 "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n\n" ++
 show cnf ++
 (if mode
   then "\n\nFinden Sie alle Fehlerhaften Wahrheitswerte in der folgenden Tabelle.\n\n"
   else "\n Gehoert die folgende Wahrheitstabelle zu der Formel?\n\n") ++
 show table ++
 (if mode
   then "\nGeben Sie die Loesung als eine Liste der fehlerhaften Indices an."
   else "\nGeben Sie als Loesung die Antwort 'ja' oder 'nein' an.")



evaluateDecide :: Bool -> IO ()
evaluateDecide bool = do
 solution <- try readLn :: IO (Either SomeException String)
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s -> case s of "ja"   -> putStrLn (if bool then "Richtige Antwort" else "Falsche Antwort")
                                       "nein" -> putStrLn (if not bool then "Richtige Antwort" else "Falsche Antwort")
                                       _      -> putStrLn "keine Lösung der Aufgabe."

evaluateDecide2 :: [Int] -> IO ()
evaluateDecide2 indices = do
 solution <- try readLn :: IO (Either SomeException [Int])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s -> let correct = fromList (map (+1) indices)
                                 solution = fromList s in
                                   if correct == solution then putStrLn "Richtige Antwort"
                                                          else putStrLn ("Ihre Loesung beinhaltet " ++ show (mistakes (toList solution) (toList correct)) ++ " Fehler.")
  where mistakes [] ys = length ys
        mistakes xs [] = length xs
        mistakes (x:xs) (y:ys) = (if x == y then 0 else 1) + mistakes xs ys



main :: IO()
main = decideExercise defaultDecideConfig
