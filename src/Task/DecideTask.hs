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
import qualified Data.Set as Set
import Test.QuickCheck (generate,elements)
import Formula (Cnf(..),genCnf,partEvalCnf,getLiterals,getCs)
import Table (Table,getTable,genWrongTable,possibleAllocations)
import Types (DecideConfig(..),CnfConfig(..),ClauseConfig(..))




solver :: Cnf -> Int
solver cnf = step allocAndCnf
  where
    allocs = possibleAllocations (getLiterals cnf)
    allocAndCnf = zip allocs (repeat cnf)
    step [] = 0
    step (([],_):xss) = step xss
    step ((x:xs,form):xss)
        | Set.null (getCs form) = step xss
        | otherwise = case partEvalCnf form x of
            Left _ -> 1 + step cascaded
            Right res -> 1 + step ((xs,res):cascaded)

          where
            cascaded = map (\(a,f) -> if x == head a then (tail a,newF x f) else (a,f)) xss
            newF y f = case partEvalCnf f y of Left _ -> Cnf Set.empty
                                               Right f2 -> f2








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