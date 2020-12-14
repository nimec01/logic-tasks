
module Table
       (
         Table
       , getTable
       , genGapTable
       , genWrongTable
       , fillGaps
       , readEntries
       , readLiterals
       , countDiffEntries
       , possibleAllocations
       ) where



import Data.List (transpose)
import Data.Maybe (isNothing)
import Data.Set (toList,unions,Set)
import Test.QuickCheck
import Formula (Allocation,Literal(..),Clause(..),Cnf(..),evalCnf)
import qualified Data.Set as Set (map,fromList)



data Table = Table
    { getLiterals :: [Literal]
    , getEntries :: [Maybe Bool]} deriving Ord

instance Eq Table where
    (==) t1 t2 = getEntries t1 == getEntries t2



instance Show Table where
    show t = header ++ "\n" ++ rows
      where
        literals = getLiterals t

        formatLine :: Show a => [a] -> Maybe Bool -> String
        formatLine [] _ = []
        formatLine x y =
            foldr ((\a b -> a ++ " | " ++ b) . show) (maybe "---" show y) x ++ "\n"

        header = concat [show x ++ " | " | x <- literals] ++ "VALUES"
        rows = concat [formatLine x y | (x,y) <- unformattedRows]
          where
            unformattedRows = zip (transpose $ comb (length literals) 1) $ getEntries t
              where
                comb :: Int -> Int -> [[Int]]
                comb 0 _ = []
                comb len n =
                    concat (replicate n $ repNum 0 ++ repNum 1) : comb (len-1) (n*2)
                  where
                    num = 2^(len -1)

                    repNum :: a -> [a]
                    repNum = replicate num



instance Arbitrary Table where
    arbitrary = sized table
      where
        table :: Int -> Gen Table
        table n = do
            cnf <- resize n arbitrary
            pure (getTable cnf)



getTable :: Cnf -> Table
getTable cnf = Table literals values
  where
    literals = toList $ unions $ map (Set.map filterSign . getLs) $ toList (getCs cnf)

    filterSign :: Literal -> Literal
    filterSign x = case x of Not y -> Literal y
                             _     -> x
    values = map (`evalCnf` cnf) $ possibleAllocations literals






allCombinations :: [Literal] -> Int ->  [Allocation]
allCombinations [] _ = []
allCombinations (x:xs) n =
    concat (replicate n $ pairs False ++ pairs True) : allCombinations xs (n*2)
  where
    num = 2^ length xs
    pairs :: a -> [(Literal,a)]
    pairs a = replicate num (x,a)



possibleAllocations :: [Literal] -> [Allocation]
possibleAllocations xs = transpose (allCombinations xs 1)



genGapTable :: Table -> Int -> Gen Table
genGapTable table gaps
    | gaps < 0 = error "The amount of gaps is negative."
    | gaps > 100 = error "gap percentage must be less than 100%."
    | otherwise = generateGaps [] percentage
  where
    rowAmount = length (getEntries table)
    percentage = maximum [gaps * rowAmount `div` 100,1]

    generateGaps :: [Int] -> Int -> Gen Table
    generateGaps indices 0 = do
        let
          entries = getEntries table
          newEntries = [ if x `elem` indices then Nothing else entries !! x
                       | x <- [0..length entries-1]]
          gapTable = Table (getLiterals table) newEntries

        pure gapTable

    generateGaps indices num = do
        rInt <- suchThat (chooseInt (0, length (getEntries table)-1)) (`notElem` indices)
        generateGaps (rInt: indices) (num-1)




genWrongTable :: Table -> Int -> Gen ([Int],Table)
genWrongTable table changes
    | changes < 0 = error "The amount of changes is negative."
    | rowAmount < changes = genWrongTable table rowAmount
    | otherwise = generateChanges [] changes
  where
    rowAmount = length (getEntries table)

    generateChanges :: [Int] -> Int -> Gen ([Int],Table)
    generateChanges indices 0 = do
        let
          entries = getEntries table

          at :: Int -> Maybe Bool
          at index = entries !! index

          newEntries = [ if x `elem` indices then not <$> at x else at x
                       | x <- [0..length entries-1]]
          newTable = Table (getLiterals table) newEntries

        pure (indices,newTable)

    generateChanges indices num = do
        rInt <- suchThat (chooseInt (0, length (getEntries table)-1)) (`notElem` indices)
        generateChanges (rInt: indices) (num-1)



fillGaps :: [Bool] -> Table -> Table
fillGaps solution table
    | length solution > length (filter isNothing tabEntries) = table
    | otherwise = Table (getLiterals table) (filledIn solution tabEntries)
  where
    tabEntries = getEntries table

    filledIn :: [Bool] -> [Maybe Bool] -> [Maybe Bool]
    filledIn [] ys = ys
    filledIn _ [] = []
    filledIn (x:xs) (y:ys) = if isNothing y
        then Just x : filledIn xs ys
        else y : filledIn (x:xs) ys



readEntries :: Table -> [Maybe Bool]
readEntries = getEntries

readLiterals :: Table -> Set Literal
readLiterals = Set.fromList . getLiterals

countDiffEntries :: Table -> Table -> Int
countDiffEntries t1 t2 = diffs (getEntries t1) (getEntries t2)
  where
    diffs :: [Maybe Bool] -> [Maybe Bool] -> Int
    diffs [] ys = length ys
    diffs xs [] = length xs
    diffs (x:xs) (y:ys) = (if x == y then 0 else 1) + diffs xs ys
