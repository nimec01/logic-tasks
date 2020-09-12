module Main where



import Types (GiveCnfConfig,defaultGiveCnfConfig,checkGiveCnfConfig)
import Task.GiveCnfTask
import Task.Utility (ensureChecksAndExecute)



giveCnfExercise :: GiveCnfConfig -> IO()
giveCnfExercise = ensureChecksAndExecute checkGiveCnfConfig executeCnfExercise
  where
    executeCnfExercise cnfConfig = do
        (desc,table) <- genGiveCnfExercise cnfConfig
        putStrLn desc
        evaluateCnf table



main = giveCnfExercise defaultGiveCnfConfig
