import Formula
import Data.List (delete,nub,sort)


resolve :: Clause -> Clause -> Clause
resolve (Clause x) (Clause y) = Clause $ nub $ removeOpposites x y
 where removeOpposites xs [] = xs
       removeOpposites [] ys = ys
       removeOpposites (x:xs) ys = if (opposite x) `elem` ys then removeOpposites xs $ delete (opposite x) ys
                                                             else x : removeOpposites xs ys
 

canResolve :: Clause -> Clause -> Bool
canResolve c1 c2 = not $ null [x | x <- getLs c1, opposite x `elem` getLs c2]


doResolutionStep :: CNF -> (Clause,Clause) -> CNF
doResolutionStep cnf (c1,c2) = if canResolve c1 c2 && sort (getLs (newClause)) `notElem` map sort (map getLs oldClauses)  then CNF (oldClauses ++ [newClause]) else cnf 
 where newClause = resolve c1 c2
       oldClauses = getCs cnf


applySteps :: CNF -> [(Clause,Clause)] -> CNF
applySteps cnf [] = cnf
applySteps cnf (x:xs) = applySteps (doResolutionStep cnf x) xs


tryResolution :: CNF -> Bool
tryResolution (CNF []) = True
tryResolution cnf = resolve [cnf]
 where resolve cnf = if null cnf then False else if or $ map (\cnf -> Clause [] `elem` getCs cnf) nextStep then True else resolve nextStep
        where nextStep = removeDupesCNFs [new | xs <- cnf, x <- getCs xs, y <- getCs xs, y /=x, let new = doResolutionStep xs (x,y), new /= xs]

test = CNF [Clause [Literal 'A',Not 'B'], Clause [Not 'A', Literal 'C'], Clause [Literal 'B', Not 'C']]

printList :: Show a => [a] -> IO ()
printList [] = putStrLn "nope" 
printList (x:xs) = do {print x; printList xs} 


main = do
 print test
 putStrLn "\n"
 print $ doResolutionStep step1 (getCs step1  !! 3, getCs step1 !! 1)
  where step1 = doResolutionStep test (getCs test !! 0, getCs test !! 2)
