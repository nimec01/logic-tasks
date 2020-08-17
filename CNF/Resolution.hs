module Resolution
       (
         genRes
       , resolve
       , applySteps
       , showResClauses 
       ) where


import Data.List (delete,nub,sort,(\\))
import Data.Maybe (fromJust, isJust)
import Test.QuickCheck (Gen,chooseInt,elements)
import Formula (Clause(..),Literal(..),opposite, removeDupesClauses, isElem)




minSteps :: [[Clause]] -> Maybe Int
minSteps [] = Just 0
minSteps xs = Just $ minimum (map length (allSteps xs)) - length ( head xs) 
allSteps :: [[Clause]] -> [[Clause]]
allSteps [] = []
allSteps xs 
 | any (\x -> Clause [] `elem` x) xs = xs
 | otherwise = allSteps (oneStep xs) 
oneStep :: [[Clause]] -> [[Clause]]
oneStep [] = []
oneStep (x:xs) = [step : x | step <- possible, not (isElem step x)] ++ oneStep xs
 where possible = removeDupesClauses $ map fromJust (filter isJust [resolve y z a | y <- x, z <- x, a <- getLs y, y /= z])  


resolve :: Clause -> Clause -> Literal -> Maybe Clause
resolve (Clause x) (Clause y) literal
  | literal `elem` x = if opposite literal `elem` y then Just (Clause (nub (x ++ y) \\ [literal,opposite literal])) else Nothing
  | literal `elem` y = resolve (Clause y) (Clause x) literal
  | otherwise = Nothing



genRes :: (Int,Int) -> Int -> [Char] -> Gen [Clause]
genRes (minLen,maxLen) steps lits = do
  clauses <- buildClauses lits [] []            
  return (map Clause (nub (map sort clauses)))


 where buildClauses xs ys zs
        | length ys  >= steps+1  = return ys
        | otherwise =       do case ys of []     -> do chosenChar <- elements xs
                                                       buildClauses xs ([Literal chosenChar] : [[Not chosenChar]]) ([Literal chosenChar] : [[Not chosenChar]])
                                          _      -> do let underMin = filter (\clause -> length clause < minLen) ys
                                                       let underMax = filter (\clause -> length clause <= maxLen) ys
                                                       chosenClause <- elements (if null underMin then underMax else underMin)
                                                       let chooseableLits = filter (\lit -> Literal lit `notElem` chosenClause && Not lit `notElem` chosenClause) xs
                                                       choice <- if length chosenClause == 1 then return 1 else 
                                                                   if length chosenClause == maxLen then return 2 else chooseInt (1,2)
                                                       if choice == 1 then do chosenChar <- elements chooseableLits
                                                                              buildClauses xs (nub (map sort ((Literal chosenChar : chosenClause) : (Not chosenChar : chosenClause) : delete chosenClause ys))) ((Literal chosenChar : chosenClause) : (Not chosenChar : chosenClause) : zs)
                                                                      else do chosenChar <- elements xs
                                                                              firstAmount <- chooseInt (1,length chosenClause-1)
                                                                              chosenSign <- elements [Literal chosenChar, Not chosenChar]
                                                                              let newClause1 = chosenSign : take firstAmount chosenClause
                                                                              let newClause2 = opposite chosenSign : drop firstAmount chosenClause
                                                                              let newSet = nub (map sort (newClause1 : newClause2 : delete chosenClause ys))
                                                                              let possible = removeDupesClauses $ map fromJust (filter isJust ([resolve (Clause newClause1) (Clause z) y | y <- newClause1, z <- newSet, z /= newClause2, z /= newClause1] ++ [resolve (Clause newClause2) (Clause z) y | y <- newClause2, z <- newSet, z /= newClause2, z /= newClause1])) 
                                                                              if any (\cl -> getLs cl `elem` zs) possible
                                                                                then buildClauses xs ys zs
                                                                                else buildClauses xs newSet (newClause1 : newClause2 :zs)



applyStep :: [(Int,Clause)] -> (Int,Int,Literal) -> Maybe [(Int,Clause)]
applyStep [] _ = Just []
applyStep xs (i1,i2,literal) = case lookup i1 xs of
                                 Just c1 -> case lookup i2 xs of
                                              Just c2 -> case resolve c1 c2 literal of
                                                          Just newClause -> Just ((newIndex, newClause) : xs)
                                                          Nothing        -> Nothing
                                              Nothing -> Nothing
                                 Nothing -> Nothing
 where newIndex = maximum (map fst xs) +1                                                        


applySteps :: [(Int,Clause)] -> [(Int,Int,Literal)] -> Maybe [(Int,Clause)]
applySteps [] _ = Just []
applySteps xs [] = Just xs
applySteps xs (y:ys) = case applyStep xs y of Just result -> applySteps result ys
                                              Nothing     -> Nothing



showResClauses :: [(Int,Clause)] -> String
showResClauses [] = ""
showResClauses ((index,clause):xs) = show index ++ " " ++ show (getLs clause) ++ "   " ++ showResClauses xs