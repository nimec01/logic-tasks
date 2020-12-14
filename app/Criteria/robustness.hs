module Main where

import Data.List
import Task.FillTask
import Task.GiveCnfTask
import Types
import Table
import qualified Data.Set as Set
import Data.Set (Set)
import Formula



guessAnswerFill :: Int -> [[Bool]]
guessAnswerFill size =
    replicate size True : nub (rest (size-1)) ++ [replicate size False]
  where
    rest 0 = []
    rest x = permutations (replicate x True ++ replicate (size-x) False) ++ rest (x-1)



countGuesses :: [[Int]] -> [Int] -> Int
countGuesses [] _ = 0
countGuesses (x:xs) ys
 | x == ys = 1
 | otherwise = 1 + countGuesses xs ys


guessFormula :: Table -> [Cnf]
guessFormula table = map (Cnf . Set.fromList) cnfs
  where
    vars = readLiterals table
    crossProd = Set.cartesianProduct vars (Set.fromList [0,1])
    cartesianList = Set.toList $ Set.map Set.toList $ Set.powerSet crossProd
    allIn xs = all (\x -> x `elem` map fst xs) (Set.toList vars)
    toRemove xs = not (any (\(x,_) -> length (findIndices (\z -> fst z == x) xs) > 1) xs)
    possibleCs = filter allIn $ filter toRemove cartesianList
    sign = map (map (\(x,y) -> if y == 1 then x else opposite x)) possibleCs
    cnfs = sortOn length $ tail $ subsequences $ map (Clause . Set.fromList) (sort sign)



countSteps :: [Cnf] -> Table -> Int
countSteps [] _ = error "something went wrong"
countSteps (x:xs) table = if getTable x == table then 1 else 1 + countSteps xs table


eval :: Table -> Table -> [[Bool]] -> Maybe Int
eval _ _ [] = Nothing
eval table gapTable (x:xs) = if fillGaps x gapTable == table then Just 1 else (+1) <$>  eval table gapTable xs



guessFill :: Int -> [Int] -> FillConfig -> IO [Int]
guessFill 0 amount _ = return amount
guessFill x amount conf = do
    (_,(_,table,gapTable)) <- genFillExercise conf
    let gaps = maximum [1,percentageOfGaps conf * length (readEntries table) `div` 100]
        steps1 = eval table gapTable (guessAnswerFill gaps)
    case steps1 of
      Just s1 -> guessFill (x-1) (s1 : amount) conf
      Nothing -> error "something broke"


guessGive :: Int -> [Int] -> GiveCnfConfig -> IO [Int]
guessGive 0 amount _ = return amount
guessGive x amount conf = do
    (_,table) <- genGiveCnfExercise conf
    let guesses = guessFormula table
    let steps = countSteps guesses table
    guessGive (x-1) (steps : amount) conf


main = do
    conf <- readLn
    totalSteps <- guessGive 1000 [] conf
    print totalSteps

