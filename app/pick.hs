{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Main where

import Control.Exception(try,SomeException)
import Data.Set (toList)
import Test.QuickCheck (suchThat,elements,generate,vectorOf)
import Formula (CNF(..),opposite,Clause(..),genCNF)
import TaskUtility
import Types
import Table (Table,getTable)



pickExercise :: PickConfig -> IO()
pickExercise = ensureChecksAndExecute checkPickConfig executePickExercise

  where executePickExercise pickConfig = do
          (desc,genResult) <- genPickExercise pickConfig
          putStrLn desc
          case genResult of Left (zippedCnfs,table) -> evaluatePick2 zippedCnfs table
                            Right (tables,rightCnf) -> evaluatePick tables rightCnf



genPickExercise :: PickConfig -> IO (String,Either ([(Int,CNF)],Table) ([(Int,Table)],CNF))
genPickExercise PickConfig {cnfConfig, amountOfOptions, pickCnf} = do
 first <- generate getCnf
 rest <- generate (vectorOf (amountOfOptions-1) (getWithSameLiterals first))
 let cnfs = first : rest
 rightCnf <- generate (elements cnfs)
 if pickCnf 
   then do let table = getTable rightCnf
           let zippedCnfs = zip [1..] cnfs
           let desc = exerciseDescPick2 zippedCnfs table
           return (desc,Left (zippedCnfs,table))
   else do let tables = zip [1..] (map getTable cnfs)
           let desc = exerciseDescPick tables rightCnf
           return (desc,Right (tables,rightCnf))

  where clConfig = clauseConf cnfConfig
        getCnf = genCNF (minClauseAmount cnfConfig, maxClauseAmount cnfConfig) (minClauseLength clConfig, maxClauseLength clConfig) (usedLiterals clConfig)
        getWithSameLiterals x = suchThat getCnf 
            (\c -> let cLits = concatMap (toList . getLs) (toList (getCs c)) 
                       xLits = concatMap (toList . getLs) (toList (getCs x)) in 
                               all (\lit -> lit `elem` cLits || opposite lit `elem` cLits) xLits &&
                               all (\lit -> lit `elem` xLits || opposite lit `elem` xLits) cLits)
 


exerciseDescPick :: [(Int,Table)] -> CNF -> String
exerciseDescPick tables cnf =
 "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n\n" ++
 show cnf ++
 "\n Welche der folgenden Wahrheitstafeln passt zu der Formel?\n\n" ++
 showTables tables ++
 "\nGeben Sie die richtige Tafel durch ihre Nummer an."
  where showTables [] = " "
        showTables (x:xs) = show (fst x) ++ "\n" ++ show (snd x) ++ "\n" ++ showTables xs


exerciseDescPick2 :: [(Int,CNF)] -> Table -> String
exerciseDescPick2 cnfs table =
 "Betrachten Sie die folgende Wahrheitstafel: \n\n" ++
 show table ++
 "\n Welche der folgenden Formeln in konjunktiver Normalform passt zu der Wahrheitstafel?\n\n" ++
 showCnfs cnfs ++
 "\nGeben Sie die richtige Tafel durch ihre Nummer an."
  where showCnfs [] = " "
        showCnfs (x:xs) = show (fst x) ++ "\n" ++ show (snd x) ++ "\n" ++ showCnfs xs
        
        
        
evaluatePick :: [(Int,Table)] -> CNF -> IO ()
evaluatePick tables cnf = do
 solution <- try readLn :: IO (Either SomeException Int)
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s ->   putStr (case lookup s tables of Just table -> if table == getTable cnf then "Richtige Lösung" else "Falsche Lösung"
                                                               Nothing    -> "Die angegebene Tabelle existiert nicht.")

evaluatePick2 :: [(Int,CNF)] -> Table -> IO ()
evaluatePick2 cnfs table = do
 solution <- try readLn :: IO (Either SomeException Int)
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s ->   putStr (case lookup s cnfs of Just cnf -> if table == getTable cnf then "Richtige Lösung" else "Falsche Lösung"
                                                             Nothing    -> "Die angegebene Tabelle existiert nicht.")


main :: IO()
main = pickExercise defaultPickConfig