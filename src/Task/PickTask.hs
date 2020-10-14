{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}

module Task.PickTask
      ( genPickExercise
      , exerciseDescPick
      , exerciseDescPick2
      , evaluatePick
      , evaluatePick2
      ) where



import Control.Exception(try,SomeException)
import Test.QuickCheck (elements,generate,vectorOf,Gen)
import qualified SAT.MiniSat as Sat
import Formula (Cnf(..),genCnf,getLiterals,Literal(..),convert)
import Types (PickConfig(..),CnfConfig(..),ClauseConfig(..))
import Table (Table,getTable)



genPickExercise :: PickConfig -> IO (String,Either ([(Int,Cnf)],Table) ([(Int,Table)],Cnf))
genPickExercise
    PickConfig {cnfConfig = CnfConfig {clauseConf = ClauseConfig {..}, ..}, ..}
  = do
    first <- generate (getCnf usedLiterals)
    let satForm = convert first
    rest <- generate (vectorOf (amountOfOptions-1) (getWithSameLiterals first satForm))
    let cnfs = first : rest
    rightCnf <- generate (elements cnfs)
    if pickCnf
      then do
        let
          table = getTable rightCnf
          zippedCnfs = zip [1..] cnfs
          desc = exerciseDescPick2 zippedCnfs table
        pure (desc,Left (zippedCnfs,table))
      else do
        let
          tables = zip [1..] (map getTable cnfs)
          desc = exerciseDescPick tables rightCnf
        pure (desc,Right (tables,rightCnf))
  where
    getCnf :: [Char] -> Gen Cnf
    getCnf lits = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)
                     lits

    getWithSameLiterals :: Cnf -> Sat.Formula Char -> Gen Cnf
    getWithSameLiterals cnf sat = do
        let cnfLits = getLiterals cnf
        newCnf <- getCnf (map getC cnfLits)
        if getLiterals newCnf == cnfLits && Sat.satisfiable (sat Sat.:++: convert newCnf Sat.:<->: Sat.Yes)
          then pure newCnf
          else getWithSameLiterals cnf sat




exerciseDescPick :: [(Int,Table)] -> Cnf -> String
exerciseDescPick tables cnf =
    "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n\n" ++
    show cnf ++
    "\n Welche der folgenden Wahrheitstafeln passt zu der Formel?\n\n" ++
    showIndexedList tables ++
    "\nGeben Sie die richtige Tafel durch ihre Nummer an."



exerciseDescPick2 :: [(Int,Cnf)] -> Table -> String
exerciseDescPick2 cnfs table =
    "Betrachten Sie die folgende Wahrheitstafel: \n\n" ++
    show table ++
    "\n Welche der folgenden Formeln in konjunktiver Normalform passt zu der Wahrheitstafel?\n\n" ++
    showIndexedList cnfs ++
    "\nGeben Sie die richtige Tafel durch ihre Nummer an."



showIndexedList :: Show a => Show b => [(a,b)] -> String
showIndexedList [] = " "
showIndexedList (x:xs) = show (fst x) ++ "\n" ++ show (snd x) ++ "\n" ++ showIndexedList xs



evaluatePick :: [(Int,Table)] -> Cnf -> IO ()
evaluatePick tables cnf = do
    solution <- try readLn :: IO (Either SomeException Int)
    case solution of
        Left _  -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
        Right s -> putStr (checkAnswer s)
  where
    checkAnswer :: Int -> String
    checkAnswer answer = case lookup answer tables of
        Just table -> if table == getTable cnf
          then "Richtige Lösung"
          else "Falsche Lösung"
        Nothing    -> "Die angegebene Tabelle existiert nicht."



evaluatePick2 :: [(Int,Cnf)] -> Table -> IO ()
evaluatePick2 cnfs table = do
    solution <- try readLn :: IO (Either SomeException Int)
    case solution of
        Left _  -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
        Right s -> putStr (checkAnswer s)
  where
    checkAnswer :: Int -> String
    checkAnswer answer = case lookup answer cnfs of
        Just cnf -> if table == getTable cnf
          then "Richtige Lösung"
          else "Falsche Lösung"
        Nothing    -> "Die angegebene Tabelle existiert nicht."