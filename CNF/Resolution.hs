module Resolution where
import Formula
import Data.List (delete,nub,sort,(\\))
import Test.QuickCheck
import Data.Maybe



resolve :: Clause -> Clause -> Maybe Clause
resolve (Clause x) (Clause y) = if any (\x-> x `elem` oppositesX) y then Just $ Clause $ removeOpposites x y else Nothing
 where oppositesX = map opposite x
       oppositesY = map opposite y
       removeOpposites xs ys = (y \\ oppositesX) ++ (x \\ oppositesY)
       




doResolutionStep :: CNF -> (Clause,Clause) -> CNF
doResolutionStep cnf (c1,c2) = case resolve c1 c2 of Nothing  -> cnf
                                                     Just new -> if sort (getLs new) `notElem` map sort (map getLs oldClauses)  then CNF (oldClauses ++ [new]) else cnf 
 where oldClauses = getCs cnf


applySteps :: CNF -> [(Clause,Clause)] -> CNF
applySteps cnf [] = cnf
applySteps cnf (x:xs) = applySteps (doResolutionStep cnf x) xs


tryResolution :: CNF -> Maybe [CNF]
tryResolution (CNF []) = Just [CNF []]
tryResolution cnf = resolve [cnf]
 where resolve cnf = if null cnf then Nothing else if or $ map (\cnf -> Clause [] `elem` getCs cnf) nextStep then Just nextStep else resolve nextStep
        where nextStep = removeDupesCNFs [new | xs <- cnf, x <- getCs xs, y <- getCs xs, y /=x, let new = doResolutionStep xs (x,y), new /= xs]


genRes :: (Int,Int) -> (Int,Int) -> [Char] -> Int -> Int ->  Gen CNF
genRes (minNum,maxNum) (minLen,maxLen) lits minSteps maxSteps = 
 suchThat (genCNF (minNum,maxNum) (minLen,maxLen) lits) 
 (\x -> let (result,steps) = (tryResolution x, length (getCs (head(fromJust result))) - length (getCs x)) in isJust result  && 
 steps >= minSteps && steps <= maxSteps)
 


validateSteps :: [(Int,Int)] -> CNF -> Bool
validateSteps [] cnf = Clause [] `elem` (getCs cnf)
validateSteps ((i1,i2):xs) cnf = case (resolve (clauses !! i1 ) (clauses !! i2 )) of Nothing   -> False
                                                                                     Just step -> validateSteps xs (CNF (getCs cnf ++ [step])) 
 where clauses = getCs cnf

main = do
 cnf <- generate (genRes (6,6) (1,2) "ABCDEFGI" 2 2)
 print cnf
 putStrLn "\n"
 print $ tryResolution cnf
