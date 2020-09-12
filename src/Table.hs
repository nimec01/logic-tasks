
module Table
       (
         Table
       , getTable
       , genGapTable
       , genWrongTable
       , fillGaps
       , readEntries
       , countDiffEntries
       , possibleAllocations
       ) where



import Data.List (transpose)
import Data.Maybe (isNothing)
import Data.Set (toList,unions)
import Test.QuickCheck
import Formula (Allocation,Literal(..),Clause(..),Cnf(..),evalCnf)
import qualified Data.Set as Set (map)



data Table = Table
    { getLiterals :: [Literal]
    , getEntries :: [Maybe Bool]}
    deriving Eq



instance Show Table where
    show t = header ++ "\n" ++ rows
      where
        literals = getLiterals t
        formatLine [] _ = []
        formatLine x y =
            foldr ((\a b -> a ++ " | " ++ b) . show) (maybe "---" show y) x ++ "\n"
        header = concat [show x ++ " | " | x <- literals] ++ "VALUES"
        rows =
            concat [formatLine x y | (x,y) <- unformattedRows]
          where
            unformattedRows = zip (transpose $ comb (length literals) 1) $ getEntries t
        comb 0 _ = []
        comb len n =
            concat (replicate n $ replicate num 0 ++ replicate num 1) : comb (len-1) (n*2)
          where
            num = 2^(len -1)



instance Arbitrary Table where
    arbitrary = sized table
      where
        table n = do
            cnf <- resize n arbitrary
            pure (getTable cnf)



getTable :: Cnf -> Table
getTable cnf = Table literals values
  where
    literals = toList $ unions $ map (Set.map filterSign . getLs) $ toList (getCs cnf)
    filterSign x = case x of Not y -> Literal y
                             _     -> x
    values = map (`evalCnf` cnf) $ possibleAllocations literals




allCombinations :: [Literal] -> Int ->  [Allocation]
allCombinations [] _ = []
allCombinations (x:xs) n =
    concat (replicate n $ pairs False ++ pairs True) : allCombinations xs (n*2)
  where
    num = 2^ length xs
    pairs bool = replicate num (x,bool)



possibleAllocations :: [Literal] -> [Allocation]
possibleAllocations xs = transpose (allCombinations xs 1)



genGapTable :: Table -> Int -> Gen Table
genGapTable table gaps
    | gaps < 0 = error "The amount of gaps is negative."
    | rowAmount < gaps = genGapTable table rowAmount
    | otherwise = generateGaps [] gaps
  where
    rowAmount = length (getEntries table)
    generateGaps indices 0 = do
        let entries = getEntries table
        let newEntries = [ if x `elem` indices then Nothing else entries !! x
                         | x <- [0..length entries-1]]
        let gapTable = Table (getLiterals table) newEntries
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
    generateChanges indices 0 = do
        let entries = getEntries table
        let at index = entries !! index
        let newEntries = [ if x `elem` indices then not <$> at x else at x
                         | x <- [0..length entries-1]]
        let newTable = Table (getLiterals table) newEntries
        return (indices,newTable)
    generateChanges indices num = do
        rInt <- suchThat (chooseInt (0, length (getEntries table)-1)) (`notElem` indices)
        generateChanges (rInt: indices) (num-1)



fillGaps :: [Bool] -> Table -> Table
fillGaps solution table
    | length solution > length (filter isNothing tabEntries) = table
    | otherwise = Table (getLiterals table) (filledIn solution tabEntries)
  where
    tabEntries = getEntries table
    filledIn [] ys = ys
    filledIn _ [] = []
    filledIn (x:xs) (y:ys) = if isNothing y
        then Just x : filledIn xs ys
        else y : filledIn (x:xs) ys



readEntries :: Table -> [Maybe Bool]
readEntries = getEntries



countDiffEntries :: Table -> Table -> Int
countDiffEntries t1 t2 = diffs (getEntries t1) (getEntries t2)
  where
    diffs [] ys = length ys
    diffs xs [] = length xs
    diffs (x:xs) (y:ys) = (if x == y then 0 else 1) + diffs xs ys
