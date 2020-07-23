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
 putStrLn "Geben Sie die resultierende Klausel als Liste von Literalen an."
 
 
evaluateStep :: Clause -> Clause -> IO()
evaluateStep c1 c2 = do
 solution <- try readLn :: IO (Either SomeException [Literal])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form" 
                  Right s -> case resolve c1 c2 of Just res -> if res == Clause s then putStrLn "Richtige Lösung"
                                                                                  else putStrLn "Falsche Lösung"
                                                   _        -> error "Klauseln sind nicht resolvierbar "
                                                   
                                                   
                                                   
main = do
 clauses <- generate (genRes 2 3 1 "ABCD")
 exerciseDescStep (clauses !! 0) (clauses !! 1) 
 evaluateStep (clauses !! 0) (clauses !! 1)   