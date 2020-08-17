{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Cnf 
      ( FillConfig
      , CnfConfig
      , PickConfig
      , defaultFillConfig
      , defaultCnfConfig
      , defaultPickConfig
      , fillExercise
      , cnfExercise
      , pickExercise
      , checkFillConfig
      , checkCnfConfig
      , checkPickConfig
      ) where

import Control.Exception (try,SomeException)
import Test.QuickCheck (generate,vectorOf,elements)
import Formula (Literal,CNF(..),Clause(..),genCNF)
import Table (Table,getTable,evalSolution,genGapTable,genWrongTable)


data FillConfig = FillConfig
    { minClauseAmount :: Int
    , maxClauseAmount :: Int
    , minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    , amountOfGaps :: Int
    } deriving Show
    


data CnfConfig = CnfConfig
    { minClauseAmount :: Int
    , maxClauseAmount :: Int
    , minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    } deriving Show
    


data PickConfig = PickConfig
    { minClauseAmount :: Int
    , maxClauseAmount :: Int
    , minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    , amountOfOptions :: Int
    } deriving Show


data DecideConfig = DecideConfig
    { minClauseAmount :: Int
    , maxClauseAmount :: Int
    , minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    , changes :: Int
    } deriving Show


defaultFillConfig :: FillConfig
defaultFillConfig = FillConfig
  { minClauseAmount = 2
  , maxClauseAmount = 3
  , minClauseLength = 2
  , maxClauseLength = 3
  , usedLiterals = "ABCD"
  , amountOfGaps = 4
  }



defaultCnfConfig :: CnfConfig
defaultCnfConfig = CnfConfig
  { minClauseAmount = 2
  , maxClauseAmount = 3
  , minClauseLength = 2
  , maxClauseLength = 3
  , usedLiterals = "ABCD"
  }



defaultPickConfig :: PickConfig
defaultPickConfig = PickConfig
  { minClauseAmount = 2
  , maxClauseAmount = 3
  , minClauseLength = 2
  , maxClauseLength = 3
  , usedLiterals = "ABCD"
  , amountOfOptions = 5
  }

defaultDecideConfig :: DecideConfig
defaultDecideConfig = DecideConfig
  { minClauseAmount = 2
  , maxClauseAmount = 3
  , minClauseLength = 2
  , maxClauseLength = 3
  , usedLiterals = "ABCD"
  , changes = 2
  }

fillExercise :: FillConfig -> IO()
fillExercise FillConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, amountOfGaps} = do
 cnf <- generate (genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals)
 let table = getTable cnf
 gapTable <- generate (genGapTable table amountOfGaps)
 exerciseDescFill cnf gapTable
 evaluateFill table gapTable
 
 
 
cnfExercise :: CnfConfig -> IO()
cnfExercise CnfConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals} = do
 cnf <- generate (genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals)
 let table = getTable cnf
 exerciseDescCnf table
 evaluateCnf table



pickExercise :: PickConfig -> IO()
pickExercise PickConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, amountOfOptions} = do
 cnfs <- generate (vectorOf amountOfOptions (genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals))
 let tables = zip [1..] (map getTable cnfs)
 rightCnf <- generate (elements cnfs)
 exerciseDescPick tables rightCnf
 evaluatePick tables rightCnf
 

decideExercise :: DecideConfig -> IO()
decideExercise DecideConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, changes} = do
 cnf <- generate (genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals)
 let rightTable = getTable cnf
 wrongTable <- generate $ genWrongTable rightTable changes
 displayTable <- generate $ elements [rightTable,wrongTable]
 exerciseDescDecide cnf displayTable
 evaluateDecide (if displayTable == rightTable then True else False)

exerciseDescFill :: CNF -> Table -> IO ()
exerciseDescFill cnf table = do
 putStrLn "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n"
 print cnf
 putStrLn "\nFüllen Sie in der zugehörigen Wahrheitstafel alle Lücken mit einem passenden Wahrheitswert (True oder False) \n"
 print table
 putStrLn "\nGeben Sie als Lösung eine Liste der fehlenden Werte an, wobei das erste Element der Liste der ersten Zeile entspricht, das zweite ELement der zweiten Zeile, etc."


exerciseDescCnf :: Table -> IO ()
exerciseDescCnf table = do
 putStrLn "Betrachten Sie die folgende Wahrheitstafel: \n"
 print table
 putStrLn "\nGeben Sie eine passende Formel in konjunktiver Normalform an, die zu der Tafel passt.\n"
 putStrLn "\nGeben Sie eine Liste von Listen von Literalen an, wobei jede innere Liste einer Klausel der KNF entspricht."


exerciseDescPick :: [(Int,Table)] -> CNF -> IO ()
exerciseDescPick tables cnf = do
 putStrLn "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n"
 print cnf
 putStrLn "\nZu welche der folgenden Wahrheitstafeln passt zu der Formel?\n"
 showTables tables
 putStrLn "\nGeben Sie die richtige Tafel durch ihre Nummer an."
  where showTables [] = return ()
        showTables (x:xs) = do putStrLn (show (fst x) ++ "\n" ++ show (snd x))
                               showTables xs


exerciseDescDecide :: CNF -> Table -> IO ()
exerciseDescDecide cnf table = do
 putStrLn "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n"
 print cnf
 putStrLn "\n Gehört die folgende Wahrheitstabelle zu der Formel?\n"
 print table
 putStrLn "\nGeben Sie als Lösung die Antwort 'ja' oder 'nein' an."

evaluateFill :: Table -> Table -> IO ()
evaluateFill table gapTable = do
 solution <- try readLn :: IO (Either SomeException [Bool])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s ->   putStr (if evalSolution s table gapTable then "Richtige Lösung" else "Falsche Lösung")


evaluateCnf :: Table -> IO ()
evaluateCnf table = do
  solution <- try readLn :: IO (Either SomeException [[Literal]])
  case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                   Right s ->   putStr (if table == getTable (CNF (map Clause s)) then "Richtige Lösung" else "Falsche Lösung")


evaluatePick :: [(Int,Table)] -> CNF -> IO ()
evaluatePick tables cnf = do
 solution <- try readLn :: IO (Either SomeException Int)
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s ->   putStr (case lookup s tables of Just table -> if table == getTable cnf then "Richtige Lösung" else "Falsche Lösung"
                                                               Nothing    -> "Die angegebene Tabelle existiert nicht.")

evaluateDecide :: Bool -> IO ()
evaluateDecide bool = do
 solution <- try readLn :: IO (Either SomeException String)
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s -> case s of "ja"   -> putStrLn (if bool then "Richtige Antwort" else "Falsche Antwort")  
                                       "nein" -> putStrLn (if not bool then "Richtige Antwort" else "Falsche Antwort")
                                       _      -> putStrLn "keine Lösung der Aufgabe."

checkFillConfig :: FillConfig -> Maybe String
checkFillConfig FillConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, amountOfGaps} 
 | any (<0) [minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength,amountOfGaps] = Just "At least one of your integer parameters is negative."
 | null usedLiterals = Just "You did not specify which literals should be used."
 | minClauseAmount > maxClauseAmount = Just "The minimum amount of clauses is greater than the maximum amount."
 | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum."
 | lengthLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
 | amountOfGaps >  2^lengthLiterals = Just "There's not enough literals for this amount of gaps."
 | amountOfGaps > 2^(maxClauseAmount*maxClauseLength) = Just "This amount of gaps is not possible with your Clause length and amount settings."
 | otherwise = Nothing
  where lengthLiterals = length usedLiterals

checkCnfConfig :: CnfConfig -> Maybe String
checkCnfConfig CnfConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals}
 | any (<0) [minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength] = Just "At least one of your integer parameters is negative."
 | null usedLiterals = Just "You did not specify which literals should be used."
 | minClauseAmount > maxClauseAmount = Just "The minimum amount of clauses is greater than the maximum amount."
 | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum."
 | length usedLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
 | otherwise = Nothing


checkPickConfig :: PickConfig -> Maybe String
checkPickConfig PickConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, amountOfOptions}
 | any (<0) [minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength,amountOfOptions] = Just "At least one of your integer parameters is negative."
 | null usedLiterals = Just "You did not specify which literals should be used."
 | minClauseAmount > maxClauseAmount = Just "The minimum amount of clauses is greater than the maximum amount."
 | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum."
 | length usedLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
 | otherwise = Nothing