
module Main where



import Task.ResolveTask
import Task.Utility (ensureChecksAndExecute)
import Types (ResolutionConfig,defaultResolutionConfig,checkResolutionConfig)



resolutionExercise :: ResolutionConfig -> IO()
resolutionExercise = ensureChecksAndExecute checkResolutionConfig executeResolutionExercise

  where executeResolutionExercise resolutionConfig = do
          (desc,numberedClauses) <- genResolutionExercise resolutionConfig
          putStrLn desc
          evaluateResolve numberedClauses



main :: IO()
main = resolutionExercise defaultResolutionConfig
