module Main where


import Types
import Task.ResolveTask
import Task.FillTask
import Task.StepTask
import Table(Table,readEntries)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Formula(Cnf(..),Clause(..),getLs,getLiterals,Literal(..))
import qualified Data.Set as Set
import Data.List(sort,nub)




features :: Table -> Cnf -> Vector Double
features table (Cnf set) = Vector.fromList [truesPercent, clauseAmount, diffLits]
  where
    entries = readEntries table
    truesLen = length (filter (== Just True) entries)
    truesPercent = fromIntegral truesLen / fromIntegral (length entries)
    diffLits = fromIntegral (length (getLiterals (Cnf set)))
    variables = concat (Set.map (Set.toList . getLs) set)
    clauseAmount = fromIntegral (Set.size set)




featuresStep :: Clause -> Clause -> Vector Double
featuresStep c1 c2 = Vector.fromList [lenDiff, diffLits, solutionLen]
  where
    lenDiff = fromIntegral (abs (Set.size (getLs c1) - Set.size (getLs c2)))
    both = Set.fromList [c1,c2]
    lits = getLiterals (Cnf both)
    diffLits = fromIntegral (length lits)
    variables = concat (Set.map (Set.toList . getLs) both)
    solutionLen = fromIntegral (length (nub variables) - 2)



featuresRes :: [Clause] -> Vector Double
featuresRes xs = Vector.fromList [avgLen, diffLits]
  where
    all = Set.fromList xs
    lits = getLiterals (Cnf all)
    diffLits = fromIntegral (length lits)
    variables = concat (Set.map (Set.toList . getLs) all)
    avgLen = fromIntegral (sum (map (Set.size . getLs) xs)) / fromIntegral (length xs)



squaredDiff :: Num a => a -> a -> a
squaredDiff x y = (x - y)^2



zeroVector :: [Vector a] -> a -> Vector a
zeroVector [] _ = Vector.empty
zeroVector xs zero = Vector.replicate vectorLength zero
   where
     vectorLength = Vector.length (head xs)


sumVector [] = Vector.empty
sumVector xs = foldl (Vector.zipWith (+)) (zeroVector xs 0)  xs



averageVector :: [Vector Double] -> Vector Double
averageVector [] = Vector.empty
averageVector xs = Vector.map (\x -> x / fromIntegral (length xs)) (sumVector xs)


variance :: [Vector Double] -> Vector Double
variance xs = Vector.map (\x -> x / fromIntegral (length xs -1)) (sumVector diffsVectors)
  where
    diffsVectors = map (Vector.zipWith squaredDiff average) xs
    average = averageVector xs



getFeaturesRes :: Int -> ResolutionConfig -> IO [Vector Double]
getFeaturesRes num conf = run num []
  where
    run 0 xs = pure xs
    run x xs = do
        (_,numClauses) <- genResolutionExercise conf
        run (x-1) (featuresRes (map snd numClauses) : xs)




getFeaturesStep :: Int -> StepConfig -> IO [Vector Double]
getFeaturesStep num conf = run num []
  where
    run 0 xs = pure xs
    run x xs = do
        (_,(c1,c2)) <- genStepExercise conf
        run (x-1) (featuresStep c1 c2 : xs)




getFeaturesTT :: Int -> FillConfig -> IO [Vector Double]
getFeaturesTT num conf = run num []
  where
    run 0 xs = pure xs
    run x xs = do
        (_,(cnf,table,_)) <- genFillExercise conf
        run (x-1) (features table cnf : xs)


normalize :: [Vector Double] -> [Vector Double]
normalize [] = []
normalize xs = map (Vector.zipWith (flip (/)) divider . Vector.zipWith (flip (-)) minVector) xs
  where
    zero = zeroVector xs 0
    greater x y = if x > y then x else y
    smaller x y = if x < y then x else y
    maxVector = foldl (Vector.zipWith greater) zero xs
    minVector = foldl (Vector.zipWith smaller) maxVector xs
    divider = Vector.zipWith (\x y -> if x == y then 1 else x-y) maxVector minVector


median :: [Vector Double] -> Vector Double
median [] = error "can not compute median for 0 instances."
median xs = Vector.fromList indMedian
  where
    toList index = [v Vector.! index | v <- xs]
    indMedian = [listMedian (toList i) | i <-[0..1]]


precision :: Int -> Double -> Double
precision digits num = fromIntegral (round $ num * (10^digits)) / (10.0^^digits)

listMedian :: [Double] -> Double
listMedian [] = error "can not compute median for the empty list."
listMedian xs =
    if odd len
      then sorted !! (len `div` 2)
      else ((sorted !! (len `div` 2 - 1)) + (sorted !! (len `div` 2))) / 2
  where
    len = length xs
    sorted = sort xs



main = do
  conf <- readLn
  featureList <- getFeaturesRes 100000 conf
  let norm = normalize featureList
      var = variance norm
      threeDigits = Vector.map (precision 3)
      avg = threeDigits (averageVector featureList)
      normAvg = threeDigits (averageVector norm)
      sdv = threeDigits (Vector.map sqrt var)
      med = threeDigits (median featureList)
      normMed = threeDigits (median norm)
  putStr "average: "
  print avg
  putStr "normed average: "
  print normAvg
  putStr "median: "
  print med
  putStr "normed median: "
  print normMed
  putStr "normed standard deviation: "
  print sdv
