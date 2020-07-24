module ResolutionTask where

import Resolution
import Formula
import Control.Exception (try,SomeException)
import Test.QuickCheck




exerciseDescStep :: Clause -> Clause -> IO()
exerciseDescStep c1 c2 = do
 putStrLn "Resolvieren Sie die folgenden Klauseln:"
 print c1
 print c2 
 putStrLn "Geben Sie das in der Resolution genutzte Literal und das Ergebnis in Form eines Tupels an: (Literal, Liste der Literale in der neuen Klausel)."
 
 
exerciseDescResolve ::  [(Int,Clause)] -> IO() 
exerciseDescResolve clauses = do
 putStrLn "Führen Sie das Resolutionsverfahren mit der folgenden Klauselmenge durch."
 putStrLn (showResClauses clauses)
 putStrLn "Geben Sie die Lösung als eine Liste von Tripeln an, wobei die Tripel nach dem Muster (Erster Index, Zweiter Index, ausgewähltes Literal) aufgebaut sind."
 putStrLn "Neu resolvierte Klauseln erhalten dabei fortlaufend den nächst höheren Index."
 
 
evaluateResolve :: [(Int,Clause)] -> IO()
evaluateResolve clauses = do
 solution <- try readLn :: IO (Either SomeException [(Int,Int,Literal)])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form" 
                  Right s -> case applySteps clauses s of Just result -> if Clause [] `elem` (map snd result) then putStrLn "Richtige Lösung"
                                                                                                              else putStrLn "Falsche Lösung"
                                                          _           -> error "Falsches Ergebnis, die leere Klausel wurde nicht resolviert."
 
 
 
 
evaluateStep :: Clause -> Clause -> IO()
evaluateStep c1 c2 = do
 solution <- try readLn :: IO (Either SomeException (Literal,[Literal]))
 case solution of Left e                 -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form" 
                  Right (literal,result) -> case resolve c1 c2 literal of Just res -> if res == Clause result then putStrLn "Richtige Lösung"
                                                                                                       else putStrLn "Falsche Lösung"
                                                                          Nothing  -> error "Klauseln sind nicht resolvierbar "
test:: [(Int,Clause)]           
test = zip [1..] [Clause [Not 'A'], Clause [Literal 'A',Not 'C'], Clause [Literal 'C']]
           
                                                   
                                                   
main = do
 clauses <- generate (genRes 3 3 3 "ABCD")
 let numberedClauses = zip [1..] clauses
 exerciseDescResolve numberedClauses 
 evaluateResolve numberedClauses
   