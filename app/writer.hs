

module Main where



import Types
import Task.Utility
import Task.FillTask
import Task.DecideTask
import Task.GiveCnfTask
import Task.StepTask
import Task.ResolveTask
import Task.PickTask


main :: IO ()
main = do
 putStrLn "how many?"
 amount <- readLn
 writeExercises amount defaultFillConfig genFillExercise
 writeExercises amount defaultGiveCnfConfig genGiveCnfExercise
 writeExercises amount defaultPickConfig genPickExercise
 writeExercises amount defaultDecideConfig genDecideExercise
 writeExercises amount defaultStepConfig genStepExercise
 writeExercises amount defaultResolutionConfig genResolutionExercise


