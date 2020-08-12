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
      choice <- if steps <= 2 then return 1 else chooseInteger (1,2)
      case choice of 1 -> do clauses <- buildClauses lits []
                             let used = nub (concat clauses) 
                              -- filledClauses <- fill clauses
                             return (map Clause clauses)
                     2 -> do firstLen <- elements [1..steps-2]
                             let secLen = (steps-1)-firstLen
                             shuffledLits <- shuffle lits 
                             let lits1 = take firstLen shuffledLits
                             let lits2 = take secLen (drop firstLen shuffledLits)
                             clauses1 <- genRes 1 firstLen  firstLen lits1
                             clauses2 <- genRes 1 secLen secLen lits2
                             chosenChar <- elements (drop (secLen + firstLen) shuffledLits)                              
                             chosenSign <- elements [Literal chosenChar, Not chosenChar]
                             chosenClause1 <- elements clauses1
                             chosenClause2 <- elements clauses2
                             let litAdded1 = [y |x <- clauses1, let y = if x == chosenClause1 then Clause (chosenSign : getLs x) else x]
                             let litAdded2 = [y |x <- clauses2, let y = if x == chosenClause2 then Clause (opposite chosenSign : getLs x) else x]  
                             return (litAdded1 ++ litAdded2)  
               


                     
 where buildClauses xs ys 
        | length ys >= steps +1 = return ys 
        | otherwise =       do chosenChar <- elements xs
                               chosenLit <- elements [Literal chosenChar, Not chosenChar]
                               let xs' = delete chosenChar xs
                               case ys of []     -> buildClauses xs' ([Literal chosenChar] : [[Not chosenChar]])
                                          _      -> do chosenClause <- elements (filter (\x -> length x < len) ys) 
                                                       buildClauses xs' ([opposite chosenLit] : [ y | x <- ys, let y = if x == chosenClause then chosenLit : x else x])
           

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


 