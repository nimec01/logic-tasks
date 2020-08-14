{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module ResolutionTask where


import Control.Exception (try,SomeException)
import Data.List (sort)
import Test.QuickCheck (generate, elements)
import Formula (Clause(..),Literal(..))
import Resolution (genRes,resolve,applySteps,showResClauses)


data StepConfig = StepConfig
    { minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    } deriving Show



data ResolutionConfig = ResolutionConfig
    { minClauseLength :: Int
    , maxClauseLength :: Int
    , steps :: Int
    , usedLiterals :: [Char]
    } deriving Show



defaultResolutionConfig :: ResolutionConfig
defaultResolutionConfig = ResolutionConfig
  { minClauseLength = 1
  , maxClauseLength = 3
  , steps = 4
  , usedLiterals = "ABCD"
  }

defaultStepConfig :: StepConfig
defaultStepConfig = StepConfig 
  { minClauseLength = 1
  , maxClauseLength = 4
  , usedLiterals = "ABCDE"
  }



stepExercise :: StepConfig -> IO()
stepExercise StepConfig { minClauseLength, maxClauseLength, usedLiterals} = do
 clauses <- generate (genRes (minClauseLength, maxClauseLength) (maxClauseLength+1) usedLiterals)
 clause1 <- generate (elements clauses)
 let resolvable = filter (\x -> any (/=Nothing) [resolve clause1 x z | z <- getLs clause1]) clauses 
 clause2 <- generate (elements resolvable)
 putStrLn (exerciseDescStep clause1 clause2)
 evaluateStep clause1 clause2



resolutionExercise :: ResolutionConfig -> IO()
resolutionExercise ResolutionConfig { minClauseLength, maxClauseLength, steps, usedLiterals} = do
 clauses <- generate (genRes (minClauseLength,maxClauseLength) steps usedLiterals)
 let numberedClauses = zip [1..] clauses
 putStr (exerciseDescResolve numberedClauses)
 evaluateResolve numberedClauses



exerciseDescStep :: Clause -> Clause -> String
exerciseDescStep c1 c2 = "Resolvieren Sie die folgenden Klauseln:\n" ++ show c1 ++ "\n" ++ show c2 ++ "\n" ++
 "Geben Sie das in der Resolution genutzte Literal und das Ergebnis in Form eines Tupels an: (Literal, Liste der Literale in der neuen Klausel)."


exerciseDescResolve ::  [(Int,Clause)] -> String
exerciseDescResolve clauses = "Fuehren Sie das Resolutionsverfahren mit der folgenden Klauselmenge durch.\n" ++
 showResClauses clauses ++ "\n" ++
 "Geben Sie die Loesung als eine Liste von Tripeln an, wobei die Tripel nach dem Muster (Erster Index, Zweiter Index, ausgewähltes Literal) aufgebaut sind.\n" ++
 "Neu resolvierte Klauseln erhalten dabei fortlaufend den naechst hoeheren Index.\n"


evaluateResolve :: [(Int,Clause)] -> IO()
evaluateResolve clauses = do
 solution <- try readLn :: IO (Either SomeException [(Int,Int,Literal)])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s -> case applySteps clauses s of Just result -> if Clause [] `elem` map snd result then putStrLn "Richtige Lösung"
                                                                                                            else putStrLn "Falsche Lösung"
                                                          _           -> error "Falsches Ergebnis, die leere Klausel wurde nicht resolviert."



evaluateStep :: Clause -> Clause -> IO()
evaluateStep c1 c2 = do
 solution <- try readLn :: IO (Either SomeException (Literal,[Literal]))
 case solution of Left e                 -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right (literal,result) -> case resolve c1 c2 literal of Just (Clause res) -> if sort res == sort result then putStrLn "Richtige Lösung"
                                                                                                       else putStrLn "Falsche Lösung"
                                                                          Nothing  -> error "Klauseln sind nicht resolvierbar "


writeExercises :: Int -> Int -> (Int,Int) -> Int -> [Char] -> IO()                                         
writeExercises count amount (minLen,maxLen) steps lits = write 1

 where write current
        | current > count = return ()
        | otherwise = do
         --amount <- generate $ chooseInt (1,maxAmount)
         --len <- generate $ chooseInt (1,maxLen)
         --steps <- generate $ chooseInt (1,maxSteps)
         --litAmount <- generate $ chooseInt (steps,maxLits)  
         --let lits = take litAmount ['A'..'Z']
                  clauses <- generate (genRes (minLen,maxLen) steps lits)
                  let numberedClauses = zip [1..] clauses
                  appendFile "exercisetest.txt" (show (current) ++"\n" ++ exerciseDescResolve numberedClauses ++"\n")
                  write (current+1)