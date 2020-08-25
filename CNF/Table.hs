
module Table 
       (
         Table
       , getTable
       , genGapTable
       , genWrongTable
       , evalSolution
       , readEntries
       ) where

import Data.List (transpose,nub,sort)
import Data.Maybe (fromJust, isNothing)
import Data.Set (toList,unions)
import Test.QuickCheck (Gen, suchThat, chooseInt)
import Formula (Allocation,Literal(..),Clause(..),CNF(..),evalCNF)
import qualified Data.Set as Set (map)



data Table = Table
    { getLiterals :: [Literal]
    , getEntries :: [Maybe Bool]}
    deriving Eq

instance Show Table where
 show t = header ++ "\n" ++ rows
  where literals = getLiterals t
        formatLine [] _ = []
        formatLine x y = foldr ((\x y -> x ++ " | " ++ y) . show) (maybe "---" show y) x ++ "\n"
        header = concat [show x ++ " | " | x <- literals] ++ "VALUES"
        rows = concat [formatLine x y | (x,y) <- zip (transpose $ comb (length literals) 1) $ getEntries t]
        comb 0 _ = []
        comb len n = concat (replicate n $ replicate num 0 ++ replicate num 1) : comb (len-1) (n*2)
         where num = 2^(len -1)



getTable :: CNF -> Table
getTable cnf = Table literals values
 where literals = toList $ unions $ map (Set.map filterSign) $ map getLs $ toList (getCs cnf)
       filterSign x = case x of Not y -> Literal y
                                _     -> x
       values = map (`evalCNF` cnf) $ transpose $ allCombinations literals 1

allCombinations :: [Literal] -> Int ->  [Allocation]
allCombinations [] _ = []
allCombinations (x:xs) n = concat (replicate n $ replicate num (x,False) ++ replicate num (x,True)) : allCombinations xs (n*2)
         where num = 2^ length xs


genGapTable :: Table -> Int -> Gen Table
genGapTable table = generateGaps []
 where generateGaps indices 0 = do
        let gapTable = Table (getLiterals table) [ if x `elem` indices then Nothing else getEntries table !! x | x <- [0..length (getEntries table)-1]]
        return gapTable
       generateGaps indices num = do
        rInt <- suchThat (chooseInt (0, length (getEntries table)-1)) (`notElem` indices)
        generateGaps (rInt: indices) (num-1)

genWrongTable :: Table -> Int -> Gen ([Int],Table)
genWrongTable table = generateChanges []
 where generateChanges indices 0 = do
        let newTable = Table (getLiterals table) [ if x `elem` indices then not <$> (getEntries table !! x) else getEntries table !! x | x <- [0..length (getEntries table)-1]]
        return (indices,newTable)
       generateChanges indices num = do
        rInt <- suchThat (chooseInt (0, length (getEntries table)-1)) (`notElem` indices)
        generateChanges (rInt: indices) (num-1)

evalSolution :: [Bool] -> Table -> Table -> Bool
evalSolution solution t gapT = solution == correct
 where correct = [ fromJust x | (x,y) <- zip (getEntries t) (getEntries gapT), isNothing y]


readEntries :: Table -> [Maybe Bool]
readEntries = getEntries