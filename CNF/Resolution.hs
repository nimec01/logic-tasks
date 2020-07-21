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


genRes :: Int -> Int -> [Char] -> Gen [Clause]
genRes amount len lits 
  | len > length lits = error "The length of a Clause exceeds the amount of Literals." 
  | amount * len > product [1..length lits] = error "Your parameters do not provide enough variations."
  | otherwise = do
      clauses <- buildClauses lits []
      paddedClauses <- padClauses clauses
      return (map Clause paddedClauses)
 where buildClauses xs ys 
        | length ys >= amount = return ys 
        | otherwise =       do chosenChar <- elements xs
                               chosenLit <- elements [Literal chosenChar, Not chosenChar]
                               let xs' = delete chosenChar xs
                               case ys of []     -> buildClauses xs' ([Literal chosenChar] : [[Not chosenChar]])
                                          _      -> do chosenClause <- elements (filter (\x -> length x < len) ys) 
                                                       buildClauses xs' ([opposite chosenLit] : [ y | x <- ys, let y = if x == chosenClause then chosenLit : x else x])
       padClauses [] = return []
       padClauses (x:xs) = do options <- shuffle availableLits
                              let x' = take (len - xLen) options ++ x
                              xs' <- padClauses xs
                              return (x' : xs')                              
          where availableLits = concat xs \\ (x ++ map opposite x)
                xLen = length x          


 
{-genRes (minNum,maxNum) (minLen,maxLen) lits minSteps maxSteps = 
 suchThat (genCNF (minNum,maxNum) (minLen,maxLen) lits) 
 (\x -> let (result,steps) = (tryResolution x, length (getCs (head(fromJust result))) - length (getCs x)) in isJust result  && 
 steps >= minSteps && steps <= maxSteps)
-}


validateSteps :: [(Int,Int)] -> CNF -> Bool
validateSteps [] cnf = Clause [] `elem` getCs cnf
validateSteps ((i1,i2):xs) cnf = case resolve (clauses !! i1 ) (clauses !! i2 ) of Nothing   -> False
                                                                                   Just step -> validateSteps xs (CNF (getCs cnf ++ [step])) 
 where clauses = getCs cnf

--main = do 
 --cnf <- generate (genRes (6,6) (1,2) "ABCDEFGI" 2 2)
 --print cnf
 --putStrLn "\n"
 --print $ tryResolution cnf
