module Main where


import Types
import Task.FillTask
import Task.GiveCnfTask
import Task.DecideTask
import Table(readEntries)
import qualified Task.FillTask as Fill(solver)
import qualified Task.DecideTask as Decide(solver)


countFalse :: Int -> GiveCnfConfig -> IO [Int]
countFalse num conf = run num []
  where
    run 0 xs = pure xs
    run x xs = do
        (_,table) <- genGiveCnfExercise conf
        let steps = length (filter (== Just False) (readEntries table))
        run (x-1) (steps : xs)



runSolverDecide :: Int -> DecideConfig -> IO [Int]
runSolverDecide num conf = run num []
  where
    run 0 xs = pure xs
    run x xs = do
        (_,(cnf,_,_,_)) <- genDecideExercise conf
        let steps = Decide.solver cnf
        run (x-1) (steps : xs)



runSolverFill :: Int -> FillConfig -> IO [Int]
runSolverFill num conf = run num []
  where
    run 0 xs = pure xs
    run x xs = do
        (_,(cnf,_,gapTable)) <- genFillExercise conf
        let steps = Fill.solver cnf gapTable
        run (x-1) (steps : xs)


main = do
    conf <- readLn
    list <- runSolverFill 1000 conf
    print list

