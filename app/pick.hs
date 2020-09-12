
module Main where



import Task.PickTask
import Types (PickConfig,defaultPickConfig,checkPickConfig)
import Task.Utility (ensureChecksAndExecute)



pickExercise :: PickConfig -> IO()
pickExercise = ensureChecksAndExecute checkPickConfig executePickExercise
  where
    executePickExercise :: PickConfig -> IO ()
    executePickExercise pickConfig = do
        (desc,genResult) <- genPickExercise pickConfig
        putStrLn desc
        case genResult of
            Left (zippedCnfs,table) -> evaluatePick2 zippedCnfs table
            Right (tables,rightCnf) -> evaluatePick tables rightCnf



main :: IO ()
main = pickExercise defaultPickConfig
