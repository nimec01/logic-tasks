module Resolution
       (
         genRes
       , resolve
       , applySteps
       , showResClauses 
       ) where


import Data.List (delete,nub,sort,(\\))
import Test.QuickCheck (Gen,chooseInt,elements)
import Formula (Clause(..),Literal(..),opposite)






resolve :: Clause -> Clause -> Literal -> Maybe Clause
resolve (Clause x) (Clause y) literal
  | literal `elem` x = if opposite literal `elem` y then Just (Clause ((x ++ y) \\ [literal,opposite literal])) else Nothing
  | literal `elem` y = resolve (Clause y) (Clause x) literal
  | otherwise = Nothing



genRes :: (Int,Int) -> Int -> [Char] -> Gen [Clause]
genRes (minLen,maxLen) steps lits = do
  clauses <- buildClauses lits []              
  return (map Clause (nub (map sort clauses)))


 where buildClauses xs ys 
        | length ys >= steps +1 = return ys
        | otherwise =       do case ys of []     -> do chosenChar <- elements xs
                                                       buildClauses xs ([Literal chosenChar] : [[Not chosenChar]])
                                          _      -> do let underMin = filter (\clause -> length clause < minLen) ys
                                                       let underMax = filter (\clause -> length clause < maxLen) ys
                                                       chosenClause <- elements (if null underMin then underMax else underMin)
                                                       chosenChar <- elements (filter (\lit -> Literal lit `notElem` chosenClause && Not lit `notElem` chosenClause) xs)
                                                       choice <- if length chosenClause == 1 then return 1 else chooseInt (1,2)
                                                       if choice == 1 then buildClauses xs ((Literal chosenChar : chosenClause) : (Not chosenChar : chosenClause) : delete chosenClause ys)
                                                                      else do firstAmount <- chooseInt (1,length chosenClause-1)
                                                                              chosenSign <- elements [Literal chosenChar, Not chosenChar]
                                                                              buildClauses xs ((chosenSign : take firstAmount chosenClause) : (opposite chosenSign : drop firstAmount chosenClause) : delete chosenClause ys)



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