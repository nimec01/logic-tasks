module TaskUtility
      ( withRatio
      , ensureChecksAndExecute
      , writeExercises      
      ) where



import Formula (CNF)
import Table (readEntries, getTable)




withRatio :: Int -> (CNF -> Bool)
withRatio percentage = 
 \c -> let tableEntries = readEntries (getTable c) in 
         length (filter (==Just True) tableEntries) == (length tableEntries *percentage `div` 100)

ensureChecksAndExecute :: (a -> Maybe String) -> (a -> IO()) -> a -> IO()
ensureChecksAndExecute checker exercise config = case checker config of Just message -> putStrLn message
                                                                        Nothing      -> exercise config


writeExercises :: Int -> a -> (a -> IO (String,b)) -> IO()                                         
writeExercises amount config exercise = write 1

 where write current
        | current > amount = return ()
        | otherwise = do
                  (desc,_) <- exercise config
                  appendFile "exercisetest.txt" (show current ++"\n" ++ desc ++"\n")
                  write (current+1)