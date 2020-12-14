module Main where



import Task.FillTask
import Task.Utility (ensureChecksAndExecute)
import Types (FillConfig,defaultFillConfig,checkFillConfig)



fillExercise :: FillConfig -> IO()
fillExercise = ensureChecksAndExecute checkFillConfig executeFillExercise
  where
    executeFillExercise :: FillConfig -> IO ()
    executeFillExercise fillConfig = do
        (desc,(_,table,gapTable)) <- genFillExercise fillConfig
        putStrLn desc
        evaluateFill table gapTable



main :: IO ()
main = fillExercise defaultFillConfig
