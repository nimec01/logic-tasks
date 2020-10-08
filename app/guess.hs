module Main where

import Data.List
import Task.FillTask
import Types
import Table


guessAnswer :: Int -> [[Bool]]
guessAnswer size =
    replicate size True : replicate size False : nub (rest (size-1))
  where
    rest 0 = []
    rest x = permutations (replicate x True ++ replicate (size-x) False) ++ rest (x-1)




eval :: Table -> Table -> [[Bool]] -> Maybe Int
eval _ _ [] = Nothing
eval table gapTable (x:xs) = if fillGaps x gapTable == table then Just 1 else (+1) <$>  eval table gapTable xs



guessLoop :: Int -> Int -> IO Int
guessLoop 0 amount = return (amount)
guessLoop x amount = do
    (_,(table,gapTable)) <- genFillExercise defaultFillConfig
    let steps = eval table gapTable (guessAnswer 4)
    case steps of
      Just steps -> guessLoop (x-1) (steps+amount)
      Nothing -> error "something broke"



main = do
    let amount = 1000
    totalSteps <- guessLoop amount 0
    let average = totalSteps `div` amount
    putStrLn $ "it took " ++ show average ++ " steps on average to guess the answer."

