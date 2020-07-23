module Resolution where
import Formula
import Data.List (delete,nub,sort,(\\))
import Test.QuickCheck
import Data.Maybe



resolve :: Clause -> Clause -> Maybe Clause
resolve (Clause x) (Clause y) = if any (`elem` oppositesX) y then Just $ Clause $ removeOpposites x y else Nothing
 where oppositesX = map opposite x
       oppositesY = map opposite y
       removeOpposites xs ys = (y \\ oppositesX) ++ (x \\ oppositesY)
       




doResolutionStep :: CNF -> (Clause,Clause) -> CNF
doResolutionStep cnf (c1,c2) = case resolve c1 c2 of Nothing  -> cnf
                                                     Just new -> if sort (getLs new) `notElem` map (sort . getLs) oldClauses  then CNF (oldClauses ++ [new]) else cnf 
 where oldClauses = getCs cnf


applySteps :: CNF -> [(Clause,Clause)] -> CNF
applySteps = foldl doResolutionStep


tryResolution :: CNF -> Maybe [CNF]
tryResolution (CNF []) = Just [CNF []]
tryResolution cnf = resolve [cnf]
 where resolve cnf  
        | null cnf = Nothing 
        | any (\cnf -> Clause [] `elem` getCs cnf) nextStep = Just nextStep 
        | otherwise = resolve nextStep
         where nextStep = removeDupesCNFs [new | xs <- cnf, x <- getCs xs, y <- getCs xs, y /=x, let new = doResolutionStep xs (x,y), new /= xs]


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
                                                  



validateSteps :: [(Int,Int)] -> CNF -> Bool
validateSteps [] cnf = Clause [] `elem` getCs cnf
validateSteps ((i1,i2):xs) cnf = case resolve (clauses !! i1 ) (clauses !! i2 ) of Nothing   -> False
                                                                                   Just step -> validateSteps xs (CNF (getCs cnf ++ [step])) 
 where clauses = getCs cnf


