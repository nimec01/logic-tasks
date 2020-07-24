module Resolution where
import Formula
import Data.List (delete,nub,sort,(\\))
import Test.QuickCheck
import Data.Maybe



resolve :: Clause -> Clause -> Literal -> Maybe Clause
resolve (Clause x) (Clause y) literal
  | literal `elem` x = if opposite literal `elem` y then Just (Clause ((x ++ y) \\ [literal,opposite literal])) else Nothing
  | literal `elem` y = resolve (Clause y) (Clause x) literal
  | otherwise = Nothing  
       



genRes :: Int -> Int -> Int -> [Char] -> Gen [Clause]
genRes amount len steps lits 
  | len > length lits = error "The length of a Clause exceeds the amount of Literals." 
  | amount * len > product [1..length lits] = error "Your parameters do not provide enough variations."
  | otherwise = do
      clauses <- buildClauses lits []
      let used = nub (concat clauses) 
--      paddedClauses <- padClauses clauses used
      filledClauses <- fill clauses
      return (map Clause filledClauses)
 where buildClauses xs ys 
        | length ys >= steps +1 = return ys 
        | otherwise =       do chosenChar <- elements xs
                               chosenLit <- elements [Literal chosenChar, Not chosenChar]
                               let xs' = delete chosenChar xs
                               case ys of []     -> buildClauses xs' ([Literal chosenChar] : [[Not chosenChar]])
                                          _      -> do chosenClause <- elements (filter (\x -> length x < len) ys) 
                                                       buildClauses xs' ([opposite chosenLit] : [ y | x <- ys, let y = if x == chosenClause then chosenLit : x else x])
       
       padClauses [] _ = return []
       padClauses (x:xs) used = do 
            let available = used \\ (x ++ map opposite x)
            shuffled <- shuffle available
            let choice = take (len - length x) shuffled
            if any (\x -> opposite x `elem` choice) choice 
              then padClauses (x:xs) used
              else do let x' = x ++ choice
                      xs' <- padClauses xs used 
                      return (x':xs')  

       fill [] = return []
       fill xs 
        | length xs >= amount = return xs        
        | otherwise = do newClause <- resize len (listOf1 (genLiteral lits))
                         if newClause `notElem` xs && nub newClause == newClause && all (\x -> opposite x `notElem` newClause) newClause then fill (newClause : xs)
                                                                                                                                         else fill xs                         
                                                  



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

