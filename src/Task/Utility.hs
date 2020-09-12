module Task.Utility
      ( withRatio
      , ensureChecksAndExecute
      , writeExercises
      ) where



import Formula (Cnf)
import Table (readEntries, getTable)




withRatio :: Int -> Cnf -> Bool
withRatio percentage cnf =
    length trueEntries == (length tableEntries *percentage `div` 100)
  where
    tableEntries = readEntries (getTable cnf)
    trueEntries = filter (== Just True) tableEntries



ensureChecksAndExecute :: (a -> Maybe String) -> (a -> IO()) -> a -> IO()
ensureChecksAndExecute checker exercise config =
    case checker config of
        Just message -> putStrLn message
        Nothing      -> exercise config



writeExercises :: Int -> a -> (a -> IO (String,b)) -> IO()
writeExercises amount config exercise = write 1
  where
    write current
        | current > amount = return ()
        | otherwise = do
            (desc,_) <- exercise config
            appendFile "exercisetest.txt" (show current ++"\n" ++ desc ++"\n")
            write (current+1)
