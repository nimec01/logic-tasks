{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Main where


import Control.Exception(try,SomeException)
import Data.List (delete)
import Data.Set (insert,fromList)
import Test.QuickCheck (suchThat,generate,elements)
import Formula (Clause(..),Literal(..),opposite,genClause)
import Resolution (resolve)
import TaskUtility
import Types








stepExercise :: StepConfig -> IO()
stepExercise = ensureChecksAndExecute checkStepConfig executeStepExercise

  where executeStepExercise stepConfig = do
          (desc,(clause1,clause2)) <- genStepExercise stepConfig
          putStrLn desc
          evaluateStep clause1 clause2



genStepExercise :: StepConfig -> IO (String,(Clause,Clause))
genStepExercise StepConfig {clauseConfig} = do
 rChar <- generate $ elements literals
 rLit <- generate $ elements [Literal rChar, Not rChar]
 let restLits = delete rChar literals
 clause1 <- generate (genClause (minLen-1,maxLen-1) restLits)
 clause2 <- generate (suchThat (genClause (minLen-1,maxLen-1) restLits)
                     (not . any (\lit -> opposite lit `elem` getLs clause1) .  getLs))
 let litAddedClause1 = Clause (insert rLit (getLs clause1))
 let litAddedClause2 = Clause (insert (opposite rLit) (getLs clause2)) 
 let desc = exerciseDescStep litAddedClause1 litAddedClause2
 return (desc,(litAddedClause1,litAddedClause2))
  where literals = usedLiterals clauseConfig
        minLen = minClauseLength clauseConfig
        maxLen = maxClauseLength clauseConfig
  

exerciseDescStep :: Clause -> Clause -> String
exerciseDescStep c1 c2 = "Resolvieren Sie die folgenden Klauseln:\n" ++ show c1 ++ "\n" ++ show c2 ++ "\n" ++
 "Geben Sie das in der Resolution genutzte Literal und das Ergebnis in Form eines Tupels an: (Literal, Liste der Literale in der neuen Klausel)."



evaluateStep :: Clause -> Clause -> IO()
evaluateStep c1 c2 = do
 solution <- try readLn :: IO (Either SomeException (Literal,[Literal]))
 case solution of Left e                 -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right (literal,result) -> case resolve c1 c2 literal of Just (Clause res) -> if res == fromList result then putStrLn "Richtige Lösung"
                                                                                                       else putStrLn "Falsche Lösung"
                                                                          Nothing  -> error "Die angegebene Lösung führt nicht zu einer Resolvenz"


main :: IO()
main = stepExercise defaultStepConfig




