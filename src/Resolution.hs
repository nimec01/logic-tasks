module Resolution
       (
         genRes
       , resolve
       , applySteps
       , showResClauses
       ) where



import Data.Maybe (catMaybes)
import Test.QuickCheck (Gen,chooseInt,elements,shuffle)
import Formula (Clause(..),Literal(..),opposite)
import Data.Set (empty,Set)
import qualified Data.Set as Set



resolve :: Clause -> Clause -> Literal -> Maybe Clause
resolve (Clause x) (Clause y) literal
    | literal `Set.member` x = if opposite literal `Set.member` y
        then Just (Clause withoutLit)
        else Nothing

    | literal `Set.member` y = resolve (Clause y) (Clause x) literal
    | otherwise = Nothing
  where
    withoutLit = Set.union x y Set.\\ Set.fromList [literal,opposite literal]



genRes :: (Int,Int) -> Int -> [Char] -> Gen [Clause]
genRes (minLen,maxLen) steps lits = do
    clauses <- buildClauses lits empty empty
    shuffled <- shuffle (Set.toList clauses)
    pure (map Clause shuffled)
  where
    buildClauses :: [Char] -> Set (Set Literal) -> Set (Set Literal) -> Gen (Set (Set Literal))
    buildClauses xs ys zs
        | Set.size ys >= steps+1  = pure ys
        | otherwise =
            if Set.null ys
              then do
                chosenChar <- elements xs
                let
                  startSet = Set.fromList [Set.singleton (Literal chosenChar),Set.singleton (Not chosenChar)]
                buildClauses xs startSet startSet
              else do
                let
                  underMin = Set.filter (\clause -> Set.size clause < minLen) ys
                  underMax = Set.filter (\clause -> Set.size clause <= maxLen) ys
                chosenClause <- elements (if Set.null underMin then Set.toList underMax else Set.toList underMin)
                let
                  chooseableLits = filter (\lit -> Literal lit `Set.notMember` chosenClause && Not lit `Set.notMember` chosenClause) xs
                  clauseSize = Set.size chosenClause
                choice <- if clauseSize == 1 || chosenClause `Set.member` underMin
                            then return 1
                            else
                              if clauseSize == maxLen
                                then return 2
                                else chooseInt (1,2)
                chosenChar <- elements chooseableLits
                if choice == 1
                  then
                    checkValidAndInsert (Literal chosenChar) chosenClause clauseSize 0
                  else do
                    firstAmount <- chooseInt (1, clauseSize-1)
                    chosenSign <- elements [Literal chosenChar, Not chosenChar]
                    checkValidAndInsert chosenSign chosenClause firstAmount firstAmount
      where
        checkValidAndInsert :: Literal -> Set Literal -> Int -> Int -> Gen (Set (Set Literal))
        checkValidAndInsert lit clause get leave = do
            let
              newClause1 = Set.insert lit (Set.take get clause)
              newClause2 = Set.insert (opposite lit) (Set.drop leave clause)
              newSet = Set.insert newClause2 (Set.insert newClause1 (Set.delete clause ys))
              possible = catMaybes ([resolve (Clause newClause1) (Clause z) y | y <- Set.toList newClause1, z <- Set.toList newSet, z /= newClause2, z /= newClause1] ++ [resolve (Clause newClause2) (Clause z) y | y <- Set.toList newClause2, z <- Set.toList newSet, z /= newClause2, z /= newClause1])

            if any (\cl -> getLs cl `Set.member` zs) possible
              then buildClauses xs ys zs
              else buildClauses xs newSet (Set.insert newClause2 (Set.insert newClause1 zs))



applyStep :: [(Int,Clause)] -> (Int,Int,Literal) -> Maybe [(Int,Clause)]
applyStep [] _ = Just []
applyStep xs (i1,i2,literal) = do
    c1 <- lookup i1 xs
    c2 <- lookup i2 xs
    newClause <- resolve c1 c2 literal
    pure ((newIndex, newClause) : xs)
  where
    newIndex = maximum (map fst xs) +1



applySteps :: [(Int,Clause)] -> [(Int,Int,Literal)] -> Maybe [(Int,Clause)]
applySteps [] _ = Just []
applySteps xs [] = Just xs
applySteps xs (y:ys) = applyStep xs y >>= flip applySteps ys



showResClauses :: [(Int,Clause)] -> String
showResClauses [] = ""
showResClauses ((index,clause):xs) =
    show index ++ " " ++ literals ++ " " ++ showResClauses xs
  where
    literals = show $ Set.toList $ getLs clause