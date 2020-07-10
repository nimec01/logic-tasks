module Cnf where
import Test.QuickCheck (generate)
import Formula
import Table
import System.IO
import Control.Exception (try,SomeException)



exerciseDesc :: CNF -> Table -> IO ()
exerciseDesc cnf table = do
 putStrLn "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n"
 print cnf
 putStrLn "\nFüllen Sie in der zugehörigen Wahrheitstafel alle Lücken mit einem passenden Wahrheitswert (True oder False) \n"
 print $ table
 putStrLn "\nGeben Sie als Lösung eine Liste der fehlenden Werte an, wobei das erste Element der Liste der ersten Zeile entspricht, das zweite ELement der zweiten Zeile, etc."



exercise :: (Int,Int) -> (Int,Int) -> Int -> [Char] -> IO ()
exercise (cnfMinLen, cnfMaxLen) (clauseMinLen,clauseMaxLen) gaps literals = do
 cnf <- generate (genCNF (clauseMinLen,clauseMaxLen) (cnfMinLen,cnfMaxLen) literals) 
 let table = getTable cnf
 gapTable <- generate (genGapTable table gaps)
 exerciseDesc cnf gapTable
 solution <- try readLn :: IO (Either SomeException [Bool])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form" 
                  Right s ->  print (evalSolution s table gapTable)


  
main :: IO ()
main = exercise (1,2) (2,3) 3 "ABCDEF"

 
    




