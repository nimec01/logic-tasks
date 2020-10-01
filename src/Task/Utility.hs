module Task.Utility
      ( withRatio
      , ensureChecksAndExecute
      , writeExercises
      , noSequences
      ) where



import Formula (Cnf)
import Table (readEntries, getTable)





withRatio :: (Int,Int) -> Cnf -> Bool
withRatio (lower,upper) cnf =
    length trueEntries <= percentage upper && length trueEntries >= percentage lower
  where
    tableEntries = readEntries (getTable cnf)
    trueEntries = filter (== Just True) tableEntries

    percentage :: Int -> Int
    percentage num = length tableEntries *num `div` 100




noSequences :: Eq a => Int -> [a] -> Bool
noSequences _ [] = True
noSequences n xs
  | length same >= n = False
  | otherwise = noSequences n diff
  where
    (same,diff) = span (== head xs) xs



ensureChecksAndExecute :: (a -> Maybe String) -> (a -> IO()) -> a -> IO()
ensureChecksAndExecute checker exercise config =
    case checker config of
        Just message -> putStrLn message
        Nothing      -> exercise config



writeExercises :: Int -> a -> (a -> IO (String,b)) -> IO()
writeExercises amount config exercise = write 1
  where
    write :: Int -> IO ()
    write current
        | current > amount = pure ()
        | otherwise = do
            (desc,_) <- exercise config
            appendFile "exercisetest.txt" (show current ++"\n" ++ desc ++"\n")
            write (current+1)
