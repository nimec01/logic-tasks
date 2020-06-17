import Test.QuickCheck
import Def
import System.IO
import System.Random
import Data.List




test :: CNF
test = CNF [Clause [Atomic 'a', Not 'b'], Clause [Not 'a', Not 'b', Atomic 'c']]

alloc = [([(Atomic 'a',False), (Atomic 'b', True)],True),([(Atomic 'a',True)],False)]

evalCNF :: Allocation -> CNF -> Maybe Bool
evalCNF xs ys = and <$> sequence clauses
 where clauses = map (evalClause xs) (getCs ys)

evalClause :: Allocation -> Clause -> Maybe Bool
evalClause xs ys = or <$> sequence atomics
 where atomics = map (evalAtomic xs) (getAs ys)

evalAtomic :: Allocation -> Atomic -> Maybe Bool
evalAtomic [] _ = Nothing
evalAtomic xs (Not y) = not <$> evalAtomic xs (Atomic y) 
evalAtomic ((x,y):xs) z = if x == z then Just y else evalAtomic xs z 


getTable :: CNF -> Table
getTable cnf = Table cnf atomics values
 where atomics = nub $ map filterSign $ concatMap getAs $ getCs cnf 
       filterSign = \x -> case x of Not y -> Atomic y 
                                    _     -> x   
       values = map (\x -> evalCNF x cnf) $ transpose $ allCombinations atomics 1

allCombinations :: [Atomic] -> Int ->  [Allocation]
allCombinations [] _ = []
allCombinations (x:xs) n = [concat $ replicate n $ replicate num (x,False) ++ replicate num (x,True)] ++ allCombinations xs (n*2) 
         where num = 2^(length xs)


main = do
 cnf <- generate (chooseAny :: Gen CNF)
 let shortcnf = CNF (take 1 (getCs cnf)) 
 --let (a,b) = random gen  
 --putStrLn $ show (a :: CNF) 
 --putStr $ show $ getTable test
 putStr $ show (getTable shortcnf)
 return ()
    




{-randomClause :: RandomGen g => [Atomic] -> (Int,Int) -> g -> Clause  
randomClause [] _ _ = []
randomClause xs (lo,hi) gen
 | lo < 0 || hi < 0 = [] 
 | lo > hi          = randomClause xs (hi,lo) gen
 | otherwise        = take num (filter (\x -> getA x `elem` (map getA xs)) rList)
  where (rand, new) = next gen 
        num = rand `mod` (hi-lo+1) + lo
        rList = randomRs (minimum xs, maximum xs) gen
-}
