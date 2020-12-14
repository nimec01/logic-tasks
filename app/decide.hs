module Main where



import Types (DecideConfig(..),defaultDecideConfig,checkDecideConfig)
import Task.DecideTask
import Task.Utility (ensureChecksAndExecute)



decideExercise :: DecideConfig -> IO()
decideExercise = ensureChecksAndExecute checkDecideConfig executeDecideExercise
  where
    executeDecideExercise :: DecideConfig -> IO ()
    executeDecideExercise decideConfig = do
        (desc,(_,rightTable,displayTable,indices)) <- genDecideExercise decideConfig
        putStrLn desc
        if mistakes
          then evaluateDecide2 indices
          else evaluateDecide (displayTable == rightTable)
      where
        mistakes = findMistakes decideConfig



main :: IO ()
main = decideExercise defaultDecideConfig
