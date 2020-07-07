import Formula
import Data.List (delete,nub,sort,(\\))



resolve :: Clause -> Clause -> Maybe Clause
resolve (Clause x) (Clause y) = if any (\x-> x `elem` oppositesX) y then Just $ Clause $ removeOpposites x y else Nothing
 where oppositesX = map opposite x
       oppositesY = map opposite y
       removeOpposites xs ys = (y \\ oppositesX) ++ (x \\ oppositesY)
       

canResolve :: Clause -> Clause -> Bool
canResolve c1 c2 = not $ null [x | x <- getLs c1, opposite x `elem` getLs c2]


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


test = CNF [Clause [Literal 'A',Not 'B'], Clause [Not 'A', Literal 'C'], Clause [Literal 'B', Not 'C']]

printList :: Show a => [a] -> IO ()
printList [] = putStrLn "nope" 
printList (x:xs) = do {print x; printList xs} 


main = do
 print test
 putStrLn "\n"
 print $ tryResolution test
