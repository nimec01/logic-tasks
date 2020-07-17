module Cnf where
import Test.QuickCheck (generate)
import Formula
import Table
import System.IO
import Control.Exception (try,SomeException)



exerciseDescFill :: CNF -> Table -> IO ()
exerciseDescFill cnf table = do
 putStrLn "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n"
 print cnf
 putStrLn "\nFüllen Sie in der zugehörigen Wahrheitstafel alle Lücken mit einem passenden Wahrheitswert (True oder False) \n"
 print $ table
 putStrLn "\nGeben Sie als Lösung eine Liste der fehlenden Werte an, wobei das erste Element der Liste der ersten Zeile entspricht, das zweite ELement der zweiten Zeile, etc."


exerciseDescCNF :: Table -> IO ()
exerciseDescCNF table = do
 putStrLn "Betrachten Sie die folgende Wahrheitstafel: \n"
 print table
 putStrLn "\nGeben Sie eine passende Formel in konjunktiver Normalform an, die zu der Tafel passt.\n"
 putStrLn "\nGeben Sie eine Liste von Listen von Literalen an, wobei jede innere Liste einer Klausel der KNF entspricht."


exerciseDescPick :: [Table] -> CNF -> IO ()
exerciseDescPick tables cnf = do
 putStrLn "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n"
 print cnf
 putStrLn "\nZu welcher der folgenden Wahrheitstafeln passt die Formel?\n"
 --showTables 1 tables
 putStrLn "\nGeben Sie die richtige die richtige Tafel durch ihre Nummer an."
 -- where showTables _ [] = return ()
  --      showTables n (x:xs) = putStrLn (show n ++ "\n" ++ show x ++ "\n" ++ showTables (n+1) xs)


evaluateFill :: Table -> Table -> IO ()
evaluateFill table gapTable = do
 solution <- try readLn :: IO (Either SomeException [Bool])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form" 
                  Right s ->   putStr (if (evalSolution s table gapTable) then "Richtige Lösung" else "Falsche Lösung")


evaluateCNF :: Table -> IO ()
evaluateCNF table = do
  solution <- try readLn :: IO (Either SomeException [[Literal]])
  case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form" 
                   Right s ->   putStr (if table == getTable (CNF (map Clause s)) then "Richtige Lösung" else "Falsche Lösung")

exercise :: (Int,Int) -> (Int,Int) -> Int -> [Char] -> IO ()
exercise (cnfMinLen, cnfMaxLen) (clauseMinLen,clauseMaxLen) gaps literals = do
 cnf <- generate (genCNF (clauseMinLen,clauseMaxLen) (cnfMinLen,cnfMaxLen) literals) 
 let table = getTable cnf
 --gapTable <- generate (genGapTable table gaps)
 exerciseDescPick [table] cnf
 evaluateCNF table


  
main :: IO ()
main = exercise (1,2) (2,3) 3 "ABCD"

 
    




