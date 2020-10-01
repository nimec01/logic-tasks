{-# LANGUAGE ExistentialQuantification #-}

module Main where



import Types
import Task.Utility
import Task.FillTask
import Task.DecideTask
import Task.GiveCnfTask
import Task.StepTask
import Task.ResolveTask
import Task.PickTask



class Exercise conf where
  makeExercise :: Int -> conf -> IO()
  defaultConfig :: conf -> conf


instance Exercise FillConfig where
  makeExercise n conf = writeExercises n conf genFillExercise
  defaultConfig _ = defaultFillConfig

instance Exercise GiveCnfConfig where
  makeExercise n conf = writeExercises n conf genGiveCnfExercise
  defaultConfig _ = defaultGiveCnfConfig

instance Exercise PickConfig where
  makeExercise n conf = writeExercises n conf genPickExercise
  defaultConfig _ = defaultPickConfig

instance Exercise DecideConfig where
  makeExercise n conf = writeExercises n conf genDecideExercise
  defaultConfig _ = defaultDecideConfig

instance Exercise ResolutionConfig where
  makeExercise n conf = writeExercises n conf genResolutionExercise
  defaultConfig _ = defaultResolutionConfig

instance Exercise StepConfig where
  makeExercise n conf = writeExercises n conf genStepExercise
  defaultConfig _ = defaultStepConfig



data GenericExercise = forall conf. Exercise conf => GExercise conf



chooseType :: Int -> String -> GenericExercise
chooseType 0 c = GExercise $ (read c :: FillConfig)
chooseType 1 c = GExercise $ (read c :: GiveCnfConfig)
chooseType 2 c = GExercise $ (read c :: PickConfig)
chooseType 3 c = GExercise $ (read c :: DecideConfig)
chooseType 4 c = GExercise $ (read c :: ResolutionConfig)
chooseType 5 c = GExercise $ (read c :: StepConfig)
chooseType _ _ = error "Number not in use."


printExercise :: Int -> Bool -> GenericExercise -> IO ()
printExercise num False (GExercise conf) = makeExercise num conf
printExercise num _ (GExercise conf) = makeExercise num (defaultConfig conf)


main :: IO ()
main = do
    putStrLn "Enter the amount of exercises to print!"
    amount <- readLn
    putStrLn "Enter the number code for your exercise!"
    number <- readLn
    putStrLn "Enter your config!"
    conf <- getLine
    let dFlag = conf == "default"
    printExercise amount dFlag (chooseType number conf)
