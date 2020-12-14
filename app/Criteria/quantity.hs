module Main where


import Types
import Table
import Formula
import Task.FillTask
import Task.StepTask
import Task.ResolveTask
import Test.QuickCheck
import Data.List (nub,sort,nubBy,permutations)
import qualified Data.Set as Set
import Data.Set (Set)



mappings :: [Clause] -> [Clause] -> [[(Literal,Literal)]]
mappings xs ys = [zip (uLits xs) perm | perm <- permutations (uLits ys)]
  where
    uLits x = Set.toList (Set.unions (map uniqueLits x))

uniqueLits :: Clause -> Set Literal
uniqueLits = Set.map turnPositive . getLs



subEq :: [Clause] -> [Clause] -> Bool
subEq xs ys
    | length xs /= length ys = False
    | negative xs /= negative ys || positive xs /= positive ys = False
    | otherwise = orderedYs `elem` [sort (map (applySub mapping) preSubXs) | mapping <- mappings preSubXs ys]
  where
     orderedXs = sort xs
     orderedYs = sort ys
     preSubZips = zip (Set.toList (Set.unions (map uniqueLits xs))) (map Literal ['a'..'z'])
     preSubXs = map (applySub preSubZips) xs
     allLits [] = []
     allLits (z:zs) = Set.toList (getLs z) ++ allLits zs
     positive a = length (filter isPositive (allLits a))
     negative a = length (allLits a) - positive a


applySub :: [(Literal,Literal)] -> Clause -> Clause
applySub [] clause = clause
applySub (x:xs) clause = applySub xs (sub clause x)
  where
    sub c tup = Clause (Set.map (replacer tup) (getLs c))
    replacer (orig,repl) lit
            | lit == orig = repl
            | lit == opposite orig = opposite repl
            | otherwise = lit




getAmountFill :: Int -> FillConfig -> IO [Table]
getAmountFill num conf = run num []
  where
    run 0 xs = pure xs
    run x xs = do
        (_,(_,table,_)) <- genFillExercise conf
        run (x-1) (if table `elem` xs then xs else table : xs)




getAmountRes :: Int -> ResolutionConfig -> IO [[Clause]]
getAmountRes num conf = run num []
  where
    run 0 xs = pure xs
    run x xs = do
        (_,numClauses) <- genResolutionExercise conf
        let clauseList = map snd numClauses
        run (x-1) (if any (subEq clauseList) xs then xs else clauseList : xs)



getAmountSteps :: Int -> StepConfig -> IO [[Clause]]
getAmountSteps num conf = run num []
  where
    run 0 xs = pure xs
    run x xs = do
        (_,(c1,c2)) <- genStepExercise conf
        let clauses = [c1,c2]
        run (x-1) (if any (subEq clauses) xs then xs else clauses : xs)




main = do
    conf <- readLn
    list <- getAmountFill 10000 conf
    print (length list)


