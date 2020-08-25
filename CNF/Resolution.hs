module Resolution
       (
         genRes
       , resolve
       , applySteps
       , showResClauses 
       ) where


import Data.Maybe (fromJust,isJust)
import Test.QuickCheck (Gen,chooseInt,elements,shuffle)
import Formula (Clause(..),Literal(..),opposite)
import Data.Set (empty,size,fromList,toList,Set,member,notMember,empty,insert,delete,union,(\\))
import qualified Data.Set as Set (filter,null,take,drop) 



resolve :: Clause -> Clause -> Literal -> Maybe Clause
resolve (Clause x) (Clause y) literal
  | literal `member` x = if opposite literal `member` y then Just (Clause (union x y \\ fromList [literal,opposite literal])) else Nothing
  | literal `member` y = resolve (Clause y) (Clause x) literal
  | otherwise = Nothing



genRes :: (Int,Int) -> Int -> [Char] -> Gen [Clause]
genRes (minLen,maxLen) steps lits = do
  clauses <- buildClauses lits empty empty
  shuffled <- shuffle (toList clauses)            
  return (map Clause shuffled)


 where buildClauses xs ys zs
        | size ys  >= steps+1  = return ys
        | otherwise =       do case Set.null ys of True -> do chosenChar <- elements xs
                                                              buildClauses xs (fromList [fromList [Literal chosenChar],fromList [Not chosenChar]]) (fromList [fromList [Literal chosenChar],fromList [Not chosenChar]])
                                                   False -> do let underMin = Set.filter (\clause -> size clause < minLen) ys
                                                               let underMax = Set.filter (\clause -> size clause <= maxLen) ys
                                                               chosenClause <- elements (if Set.null underMin then toList underMax else toList underMin)
                                                               let chooseableLits = filter (\lit -> Literal lit `notMember` chosenClause && Not lit `notMember` chosenClause) xs
                                                               choice <- if size chosenClause == 1 || chosenClause `member` underMin 
                                                                      then return 1 
                                                                      else if size chosenClause == maxLen then return 2 else chooseInt (1,2)
                                                               chosenChar <- elements chooseableLits
                                                               if choice == 1 then do let newClause1 = insert (Literal chosenChar) chosenClause
                                                                                      let newClause2 = insert (Not chosenChar) chosenClause
                                                                                      let newSet = insert newClause2 (insert newClause1 (delete chosenClause ys))
                                                                                      let possible = map fromJust (filter isJust ([resolve (Clause newClause1) (Clause z) y | y <- toList newClause1, z <- toList newSet, z /= newClause2, z /= newClause1] ++ [resolve (Clause newClause2) (Clause z) y | y <- toList newClause2, z <- toList newSet, z /= newClause2, z /= newClause1])) 
                                                                                      if any (\cl -> getLs cl `member` zs) possible
                                                                                         then buildClauses xs ys zs
                                                                                         else buildClauses xs newSet (insert newClause2 (insert newClause1 zs))
                                                                              else do firstAmount <- chooseInt (1, size chosenClause-1)
                                                                                      chosenSign <- elements [Literal chosenChar, Not chosenChar]
                                                                                      let newClause1 = insert chosenSign (Set.take firstAmount chosenClause)
                                                                                      let newClause2 = insert (opposite chosenSign) (Set.drop firstAmount chosenClause)
                                                                                      let newSet = insert newClause2 (insert newClause1 (delete chosenClause ys))
                                                                                      let possible = map fromJust (filter isJust ([resolve (Clause newClause1) (Clause z) y | y <- toList newClause1, z <- toList newSet, z /= newClause2, z /= newClause1] ++ [resolve (Clause newClause2) (Clause z) y | y <- toList newClause2, z <- toList newSet, z /= newClause2, z /= newClause1])) 
                                                                                      if any (\cl -> getLs cl `member` zs) possible
                                                                                        then buildClauses xs ys zs
                                                                                        else buildClauses xs newSet (insert newClause2 (insert newClause1 zs))



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
showResClauses ((index,clause):xs) = show index ++ " " ++ show (toList (getLs clause)) ++ "   " ++ showResClauses xs