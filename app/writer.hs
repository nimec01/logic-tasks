

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
    let writeNum = writeExercises amount
    writeNum defaultFillConfig genFillExercise
    writeNum defaultGiveCnfConfig genGiveCnfExercise
    writeNum defaultPickConfig genPickExercise
    writeNum defaultDecideConfig genDecideExercise
    writeNum defaultStepConfig genStepExercise
    writeNum defaultResolutionConfig genResolutionExercise
