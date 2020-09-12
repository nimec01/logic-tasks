
module Main where



import Task.StepTask
import Task.Utility (ensureChecksAndExecute)
import Types (StepConfig,defaultStepConfig,checkStepConfig)



stepExercise :: StepConfig -> IO()
stepExercise = ensureChecksAndExecute checkStepConfig executeStepExercise
  where
    executeStepExercise stepConfig = do
        (desc,(clause1,clause2)) <- genStepExercise stepConfig
        putStrLn desc
        evaluateStep clause1 clause2



main :: IO()
main = stepExercise defaultStepConfig
