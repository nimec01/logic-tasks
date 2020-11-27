{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}

module Task.StepTask
      ( genStepExercise
      , exerciseDescStep
      , evaluateStep
      ) where



import Control.Exception(try,SomeException)
import Data.List (delete)
import Data.Set (insert,fromList)
import Test.QuickCheck (suchThat,generate,elements)
import Formula (Clause(..),Literal(..),opposite,genClause)
import Resolution (resolve)
import Types (StepConfig(..),ClauseConfig(..))



genStepExercise :: StepConfig -> IO (String,(Clause,Clause))
genStepExercise
    StepConfig {clauseConfig = ClauseConfig{..}}
  = do
    rChar <- generate $ elements usedLiterals
    rLit <- generate $ elements [Literal rChar, Not rChar]
    let restLits = delete rChar usedLiterals
    minLen1 <- generate (elements [minClauseLength-1..maxClauseLength-1])
    minLen2 <- generate (elements [minClauseLength-1..maxClauseLength-1])
    clause1 <- generate (genClause (minLen1,maxClauseLength-1) restLits)
    clause2 <- generate (suchThat (genClause (minLen2,maxClauseLength-1) restLits)
                        (not . any (\lit -> opposite lit `elem` getLs clause1) .  getLs))
    let
      litAddedClause1 = Clause (insert rLit (getLs clause1))
      litAddedClause2 = Clause (insert (opposite rLit) (getLs clause2))
      desc = exerciseDescStep litAddedClause1 litAddedClause2
    pure (desc,(litAddedClause1,litAddedClause2))



exerciseDescStep :: Clause -> Clause -> String
exerciseDescStep c1 c2 =
    "Resolvieren Sie die folgenden Klauseln:\n" ++ show c1 ++ "\n" ++ show c2 ++ "\n" ++
    "Geben Sie das in der Resolution genutzte Literal und das Ergebnis in Form eines Tupels an: (Literal, Liste der Literale in der Reslvente)."



evaluateStep :: Clause -> Clause -> IO()
evaluateStep c1 c2 = do
    solution <- try readLn :: IO (Either SomeException (Literal,[Literal]))
    case solution of
        Left _  -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
        Right (literal,result) -> case resolve c1 c2 literal of
            Just (Clause res) -> if res == fromList result
              then putStrLn "Richtige Lösung"
              else putStrLn "Falsche Lösung"
            Nothing -> putStrLn "Das angegebene Literal kann nicht zur Resolvenz genutz werden."
