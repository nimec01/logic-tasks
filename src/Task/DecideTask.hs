{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}

module Task.DecideTask
      ( genDecideExercise
      , exerciseDescDecide
      , evaluateDecide
      , evaluateDecide2
      , solver
      ) where



import Control.Exception (try,SomeException)
import Data.Set (fromList,toList)
import Test.QuickCheck (generate,elements)
import Formula (Cnf,genCnf,partEvalCnf,getLiterals)
import Table (Table,getTable,genWrongTable,possibleAllocations)
import Types (DecideConfig(..),CnfConfig(..),ClauseConfig(..))


solver :: Cnf -> Int
solver cnf = sum $ map snd solveForAllocs
  where
    allocs = possibleAllocations (getLiterals cnf)
    solveForAllocs = map (partEvalCnf cnf) allocs



genDecideExercise :: DecideConfig -> IO (String,(Cnf,Table,Table,[Int]))
genDecideExercise
    DecideConfig {cnfConfig = CnfConfig {clauseConf = ClauseConfig{..},..},..}
  = do
    cnf <- generate (genCnf (minClauseAmount, maxClauseAmount)
                    (minClauseLength, maxClauseLength) usedLiterals)
    let rightT = getTable cnf
    (indices,wrongT) <- generate $ genWrongTable rightT amountOfChanges
    displayT <- if findMistakes then return wrongT else pickOne [rightT,wrongT]
    let desc = exerciseDescDecide cnf displayT findMistakes
    pure (desc,(cnf,rightT,displayT,indices))
  where
    pickOne :: [Table] -> IO Table
    pickOne = generate . elements



exerciseDescDecide :: Cnf -> Table -> Bool -> String
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
    case solution of
        Left _  -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
        Right s -> case s of
            "ja"   -> putStrLn (if bool then "Richtige Antwort" else "Falsche Antwort")
            "nein" -> putStrLn (if bool then "Falsche Antwort" else "Richtige Antwort")
            _      -> putStrLn "keine Lösung der Aufgabe."



evaluateDecide2 :: [Int] -> IO ()
evaluateDecide2 indices = do
    solution <- try readLn :: IO (Either SomeException [Int])
    case solution of
        Left _  -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
        Right s -> let
           answer = fromList s
           mistakeNum = mistakes (toList answer) (toList correct)
          in
           if correct == answer
             then putStrLn "Richtige Antwort"
             else putStrLn ("Ihre Loesung beinhaltet " ++ show mistakeNum ++ " Fehler.")
  where
    correct = fromList (map (+1) indices)

    mistakes :: Eq a => [a] -> [a] -> Int
    mistakes [] ys = length ys
    mistakes xs [] = length xs
    mistakes (x:xs) (y:ys) = (if x == y then 0 else 1) + mistakes xs ys