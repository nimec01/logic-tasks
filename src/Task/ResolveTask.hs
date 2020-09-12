{-# LANGUAGE DuplicateRecordFields, RecordWildCards #-}

module Task.ResolveTask
      ( genResolutionExercise
      , exerciseDescResolve
      , evaluateResolve
      ) where



import Control.Exception(try,SomeException)
import Data.Set (empty)
import Test.QuickCheck (generate)
import Formula (Clause(..),Literal(..))
import Resolution (applySteps,genRes,showResClauses)
import Types (ResolutionConfig(..),ClauseConfig(..))



genResolutionExercise :: ResolutionConfig -> IO (String,[(Int,Clause)])
genResolutionExercise
    ResolutionConfig { clauseConfig = ClauseConfig {..}, ..}
  = do
    clauses <- generate (genRes (minClauseLength, maxClauseLength) steps usedLiterals)
    let numberedClauses = zip [1..] clauses
        desc = exerciseDescResolve numberedClauses
    pure (desc,numberedClauses)



exerciseDescResolve ::  [(Int,Clause)] -> String
exerciseDescResolve clauses =
    "Fuehren Sie das Resolutionsverfahren mit der folgenden Klauselmenge durch.\n" ++
    showResClauses clauses ++ "\n" ++
    "Geben Sie die Loesung als eine Liste von Tripeln an, wobei die Tripel nach dem Muster (Erster Index, Zweiter Index, ausgewähltes Literal) aufgebaut sind.\n" ++
    "Neu resolvierte Klauseln erhalten dabei fortlaufend den naechst hoeheren Index.\n"



evaluateResolve :: [(Int,Clause)] -> IO()
evaluateResolve clauses = do
    solution <- try readLn :: IO (Either SomeException [(Int,Int,Literal)])
    case solution of
        Left _  -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
        Right s -> case applySteps clauses s of
            Just result -> if Clause empty `elem` map snd result
              then putStrLn "Richtige Lösung"
              else putStrLn "Falsche Lösung"
            _           -> error "Falsches Ergebnis, die leere Klausel wurde nicht resolviert."