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


genRes :: (Int,Int) -> (Int,Int) -> [Char] -> Int -> Gen CNF
genRes (minNum,maxNum) (minLen,maxLen) lits minsteps = suchThat (genCNF (minNum,maxNum) (minLen,maxLen) lits) (\x -> let result = tryResolution x in isJust result  && length (getCs (head(fromJust result))) - length (getCs x) >= minsteps)
 


test = CNF [Clause [Literal 'A',Not 'B'], Clause [Not 'A', Literal 'C'], Clause [Literal 'B', Not 'C']]




main = do
 cnf <- generate (genRes (6,6) (1,2) "ABCDEFGI" 2)
 print cnf
 putStrLn "\n"
 print $ tryResolution cnf
