{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Tasks 
      ( FillConfig
      , CnfConfig
      , PickConfig
      , defaultFillConfig
      , defaultCnfConfig
      , defaultPickConfig
      , fillExercise
      , cnfExercise
      , pickExercise
      , checkFillConfig
      , checkCnfConfig
      , checkPickConfig
      ) where

import Control.Exception (try,SomeException)
import Data.List(delete)
import Data.Set (empty,toList,fromList,insert)
import Test.QuickCheck (generate,vectorOf,elements, suchThat, chooseInt)
import Formula (Literal(..),CNF(..),Clause(..),genClause,genCNF,opposite)
import Table (Table,getTable,evalSolution,genGapTable,genWrongTable,readEntries)
import Resolution (genRes,resolve,applySteps,showResClauses)


data FillConfig = FillConfig
    { minClauseAmount :: Int
    , maxClauseAmount :: Int
    , minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    , amountOfGaps :: Int
    , percentTrueEntries :: Maybe (Int,Int)
    } deriving Show
    


data CnfConfig = CnfConfig
    { minClauseAmount :: Int
    , maxClauseAmount :: Int
    , minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    , percentTrueEntries :: Maybe (Int,Int)
    } deriving Show
    


data PickConfig = PickConfig
    { minClauseAmount :: Int
    , maxClauseAmount :: Int
    , minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    , amountOfOptions :: Int
    , pickCnf :: Bool 
    } deriving Show



data DecideConfig = DecideConfig
    { minClauseAmount :: Int
    , maxClauseAmount :: Int
    , minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    , amountOfChanges :: Int
    , findMistakes :: Bool
    } deriving Show



data StepConfig = StepConfig
    { minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: [Char]
    } deriving Show



data ResolutionConfig = ResolutionConfig
    { minClauseLength :: Int
    , maxClauseLength :: Int
    , steps :: Int
    , usedLiterals :: [Char]
    } deriving Show



defaultFillConfig :: FillConfig
defaultFillConfig = FillConfig
  { minClauseAmount = 1
  , maxClauseAmount = 3
  , minClauseLength = 1
  , maxClauseLength = 3
  , usedLiterals = "ABCD"
  , amountOfGaps = 2
  , percentTrueEntries = Just (10,90)
  }



defaultCnfConfig :: CnfConfig
defaultCnfConfig = CnfConfig
  { minClauseAmount = 2
  , maxClauseAmount = 2
  , minClauseLength = 1
  , maxClauseLength = 2
  , usedLiterals = "ABC"
  , percentTrueEntries = Just (50,60)
  }



defaultPickConfig :: PickConfig
defaultPickConfig = PickConfig
  { minClauseAmount = 2
  , maxClauseAmount = 3
  , minClauseLength = 1
  , maxClauseLength = 3
  , usedLiterals = "ABCD"
  , amountOfOptions = 5
  , pickCnf = True
  }



defaultDecideConfig :: DecideConfig
defaultDecideConfig = DecideConfig
  { minClauseAmount = 2
  , maxClauseAmount = 2
  , minClauseLength = 1
  , maxClauseLength = 2
  , usedLiterals = "ABCD"
  , amountOfChanges = 2
  , findMistakes = True
  }



defaultResolutionConfig :: ResolutionConfig
defaultResolutionConfig = ResolutionConfig
  { minClauseLength = 2
  , maxClauseLength = 2
  , steps = 5
  , usedLiterals = "ABC"
  }



defaultStepConfig :: StepConfig
defaultStepConfig = StepConfig 
  { minClauseLength = 2
  , maxClauseLength = 3
  , usedLiterals = "ABCD"
  }



fillExercise :: FillConfig -> IO()
fillExercise = ensureChecksAndExecute checkFillConfig executeFillExercise

  where executeFillExercise fillConfig = do
         (desc,(table,gapTable)) <- genFillExercise fillConfig
         putStrLn desc         
         evaluateFill table gapTable
          
 
 
 
cnfExercise :: CnfConfig -> IO()
cnfExercise = ensureChecksAndExecute checkCnfConfig executeCnfExercise

  where executeCnfExercise cnfConfig = do
          (desc,table) <- genCnfExercise cnfConfig
          putStrLn desc
          evaluateCnf table




pickExercise :: PickConfig -> IO()
pickExercise = ensureChecksAndExecute checkPickConfig executePickExercise

  where executePickExercise pickConfig = do
          (desc,genResult) <- genPickExercise pickConfig
          putStrLn desc
          case genResult of Left (zippedCnfs,table) -> evaluatePick2 zippedCnfs table
                            Right (tables,rightCnf) -> evaluatePick tables rightCnf

decideExercise :: DecideConfig -> IO()
decideExercise = ensureChecksAndExecute checkDecideConfig executeDecideExercise

  where executeDecideExercise decideConfig = do
          (desc,(rightTable,displayTable,indices)) <- genDecideExercise decideConfig
          putStrLn desc        
          if mistakes 
           then evaluateDecide2 indices
           else evaluateDecide (if displayTable == rightTable then True else False)
            where mistakes = findMistakes decideConfig


stepExercise :: StepConfig -> IO()
stepExercise = ensureChecksAndExecute checkStepConfig executeStepExercise

  where executeStepExercise stepConfig = do
          (desc,(clause1,clause2)) <- genStepExercise stepConfig
          putStrLn desc
          evaluateStep clause1 clause2



resolutionExercise :: ResolutionConfig -> IO()
resolutionExercise = ensureChecksAndExecute checkResolutionConfig executeResolutionExercise

  where executeResolutionExercise resolutionConfig = do
          (desc,numberedClauses) <- genResolutionExercise resolutionConfig
          putStrLn desc
          evaluateResolve numberedClauses



genFillExercise :: FillConfig -> IO (String,(Table,Table))
genFillExercise FillConfig
  { minClauseAmount
  , maxClauseAmount
  , minClauseLength
  , maxClauseLength
  , usedLiterals
  , amountOfGaps
  , percentTrueEntries
  }  = do 
 cnf <- generate (case percentTrueEntries of Just (lower,upper) -> do ratio <- chooseInt (lower,upper)
                                                                      suchThat getCNF (withRatio ratio)
                                             Nothing            -> getCNF)
 let table = getTable cnf
 gapTable <- generate (genGapTable table amountOfGaps)
 let desc = exerciseDescFill cnf gapTable  
 return (desc,(table,gapTable))
  where getCNF = genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals      



genCnfExercise :: CnfConfig -> IO (String,Table)
genCnfExercise CnfConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, percentTrueEntries} = do
 cnf <- generate (case percentTrueEntries of Just (lower,upper) -> do ratio <- chooseInt (lower,upper)
                                                                      suchThat getCNF (withRatio ratio)
                                             Nothing            -> getCNF)
 let table = getTable cnf
 let desc = exerciseDescCnf table
 return (desc,table)
  where getCNF = genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals



genPickExercise :: PickConfig -> IO (String,Either ([(Int,CNF)],Table) ([(Int,Table)],CNF))
genPickExercise PickConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, amountOfOptions, pickCnf} = do
 first <- generate getCnf
 rest <- generate (vectorOf (amountOfOptions-1) (getWithSameLiterals first))
 let cnfs = first : rest
 rightCnf <- generate (elements cnfs)
 if pickCnf 
   then do let table = getTable rightCnf
           let zippedCnfs = zip [1..] cnfs
           let desc = exerciseDescPick2 zippedCnfs table
           return (desc,Left (zippedCnfs,table))
   else do let tables = zip [1..] (map getTable cnfs)
           let desc = exerciseDescPick tables rightCnf
           return (desc,Right (tables,rightCnf))

  where getCnf = genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals
        getWithSameLiterals x = suchThat getCnf 
            (\c -> let cLits = concatMap (toList . getLs) (toList (getCs c)) 
                       xLits = concatMap (toList . getLs) (toList (getCs x)) in 
                               all (\lit -> lit `elem` cLits || opposite lit `elem` cLits) xLits &&
                               all (\lit -> lit `elem` xLits || opposite lit `elem` xLits) cLits)
 


genDecideExercise :: DecideConfig -> IO (String,(Table,Table,[Int]))
genDecideExercise DecideConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, amountOfChanges, findMistakes} = do
 cnf <- generate (genCNF (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals)
 let rightTable = getTable cnf
 (indices,wrongTable) <- generate $ genWrongTable rightTable amountOfChanges
 displayTable <- if findMistakes then return wrongTable else generate $ elements [rightTable,wrongTable]
 let desc = exerciseDescDecide cnf displayTable findMistakes
 return (desc,(rightTable,displayTable,indices))



genStepExercise :: StepConfig -> IO (String,(Clause,Clause))
genStepExercise StepConfig { minClauseLength, maxClauseLength, usedLiterals} = do
 rChar <- generate $ elements usedLiterals
 rLit <- generate $ elements [Literal rChar, Not rChar]
 let restLits = delete rChar usedLiterals
 clause1 <- generate (genClause (minClauseLength-1,maxClauseLength-1) restLits)
 clause2 <- generate (suchThat (genClause (minClauseLength-1,maxClauseLength-1) restLits)
                     (\c -> not $ any (\lit -> opposite lit `elem` getLs clause1) (getLs c)))
 let litAddedClause1 = Clause (insert rLit (getLs clause1))
 let litAddedClause2 = Clause (insert (opposite rLit) (getLs clause2)) 
 let desc = exerciseDescStep litAddedClause1 litAddedClause2
 return (desc,(litAddedClause1,litAddedClause2))



genResolutionExercise :: ResolutionConfig -> IO (String,[(Int,Clause)])
genResolutionExercise ResolutionConfig { minClauseLength, maxClauseLength, steps, usedLiterals} = do
 clauses <- generate (genRes (minClauseLength,maxClauseLength) steps usedLiterals)
 let numberedClauses = zip [1..] clauses
 let desc = exerciseDescResolve numberedClauses
 return (desc,numberedClauses)

exerciseDescFill :: CNF -> Table -> String
exerciseDescFill cnf table = 
 "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n\n" ++
 show cnf ++
 "\nFuellen Sie in der zugehoerigen Wahrheitstafel alle Luecken mit einem passenden Wahrheitswert (True oder False) \n\n" ++
 show table ++
 "\nGeben Sie als Loesung eine Liste der fehlenden Werte an, wobei das erste Element der Liste der ersten Zeile entspricht, das zweite ELement der zweiten Zeile, etc."


exerciseDescCnf :: Table -> String
exerciseDescCnf table =
 "Betrachten Sie die folgende Wahrheitstafel: \n\n" ++
 show table ++
 "\n\nGeben Sie eine zu der Tafel passende Formel in konjunktiver Normalform an. Verwenden Sie dazu Max-Terme.\n" ++
 "\nGeben Sie eine Liste von Listen von Literalen an, wobei jede innere Liste einer Klausel der KNF entspricht."


exerciseDescPick :: [(Int,Table)] -> CNF -> String
exerciseDescPick tables cnf =
 "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n\n" ++
 show cnf ++
 "\n Welche der folgenden Wahrheitstafeln passt zu der Formel?\n\n" ++
 showTables tables ++
 "\nGeben Sie die richtige Tafel durch ihre Nummer an."
  where showTables [] = " "
        showTables (x:xs) = show (fst x) ++ "\n" ++ show (snd x) ++ "\n" ++ showTables xs

exerciseDescPick2 :: [(Int,CNF)] -> Table -> String
exerciseDescPick2 cnfs table =
 "Betrachten Sie die folgende Wahrheitstafel: \n\n" ++
 show table ++
 "\n Welche der folgenden Formeln in konjunktiver Normalform passt zu der Wahrheitstafel?\n\n" ++
 showCnfs cnfs ++
 "\nGeben Sie die richtige Tafel durch ihre Nummer an."
  where showCnfs [] = " "
        showCnfs (x:xs) = show (fst x) ++ "\n" ++ show (snd x) ++ "\n" ++ showCnfs xs

exerciseDescDecide :: CNF -> Table -> Bool -> String
exerciseDescDecide cnf table mode =
 "Betrachten Sie die folgende Formel in konjunktiver Normalform: \n\n" ++
 show cnf ++
 (if mode 
   then "\n\nFinden Sie alle Fehlerhaften Wahrheitswerte in der folgenden Tabelle.\n\n"
   else "\n Gehoert die folgende Wahrheitstabelle zu der Formel?\n\n") ++
 show table ++
 (if mode 
   then "\nGeben Sie die Loesung als eine Liste der fehlerhaften Indices an."
   else "\nGeben Sie als Loesung die Antwort 'ja' oder 'nein' an.")



exerciseDescStep :: Clause -> Clause -> String
exerciseDescStep c1 c2 = "Resolvieren Sie die folgenden Klauseln:\n" ++ show c1 ++ "\n" ++ show c2 ++ "\n" ++
 "Geben Sie das in der Resolution genutzte Literal und das Ergebnis in Form eines Tupels an: (Literal, Liste der Literale in der neuen Klausel)."



exerciseDescResolve ::  [(Int,Clause)] -> String
exerciseDescResolve clauses = "Fuehren Sie das Resolutionsverfahren mit der folgenden Klauselmenge durch.\n" ++
 showResClauses clauses ++ "\n" ++
 "Geben Sie die Loesung als eine Liste von Tripeln an, wobei die Tripel nach dem Muster (Erster Index, Zweiter Index, ausgewähltes Literal) aufgebaut sind.\n" ++
 "Neu resolvierte Klauseln erhalten dabei fortlaufend den naechst hoeheren Index.\n"



evaluateFill :: Table -> Table -> IO ()
evaluateFill table gapTable = do
 solution <- try readLn :: IO (Either SomeException [Bool])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s ->   putStr (if evalSolution s table gapTable then "Richtige Lösung" else "Falsche Lösung")



evaluateCnf :: Table -> IO ()
evaluateCnf table = do
  solution <- try readLn :: IO (Either SomeException [[Literal]])
  case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                   Right s ->   putStr (if table == getTable (CNF (fromList (map (Clause . fromList) s))) then "Richtige Lösung" else "Falsche Lösung")


evaluatePick :: [(Int,Table)] -> CNF -> IO ()
evaluatePick tables cnf = do
 solution <- try readLn :: IO (Either SomeException Int)
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s ->   putStr (case lookup s tables of Just table -> if table == getTable cnf then "Richtige Lösung" else "Falsche Lösung"
                                                               Nothing    -> "Die angegebene Tabelle existiert nicht.")

evaluatePick2 :: [(Int,CNF)] -> Table -> IO ()
evaluatePick2 cnfs table = do
 solution <- try readLn :: IO (Either SomeException Int)
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s ->   putStr (case lookup s cnfs of Just cnf -> if table == getTable cnf then "Richtige Lösung" else "Falsche Lösung"
                                                             Nothing    -> "Die angegebene Tabelle existiert nicht.")

evaluateDecide :: Bool -> IO ()
evaluateDecide bool = do
 solution <- try readLn :: IO (Either SomeException String)
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s -> case s of "ja"   -> putStrLn (if bool then "Richtige Antwort" else "Falsche Antwort")  
                                       "nein" -> putStrLn (if not bool then "Richtige Antwort" else "Falsche Antwort")
                                       _      -> putStrLn "keine Lösung der Aufgabe."

evaluateDecide2 :: [Int] -> IO ()
evaluateDecide2 indices = do
 solution <- try readLn :: IO (Either SomeException [Int])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s -> if fromList (map (+1) indices) == fromList s then putStrLn "Richtige Antwort"
                                                                          else putStrLn "Falsche Antwort"  



evaluateResolve :: [(Int,Clause)] -> IO()
evaluateResolve clauses = do
 solution <- try readLn :: IO (Either SomeException [(Int,Int,Literal)])
 case solution of Left e -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right s -> case applySteps clauses s of Just result -> if Clause empty `elem` map snd result then putStrLn "Richtige Lösung"
                                                                                                            else putStrLn "Falsche Lösung"
                                                          _           -> error "Falsches Ergebnis, die leere Klausel wurde nicht resolviert."



evaluateStep :: Clause -> Clause -> IO()
evaluateStep c1 c2 = do
 solution <- try readLn :: IO (Either SomeException (Literal,[Literal]))
 case solution of Left e                 -> putStrLn "Die Eingabe entspricht nicht der vorgegebenen Form"
                  Right (literal,result) -> case resolve c1 c2 literal of Just (Clause res) -> if res == fromList result then putStrLn "Richtige Lösung"
                                                                                                       else putStrLn "Falsche Lösung"
                                                                          Nothing  -> error "Die angegebene Lösung führt nicht zu einer Resolvenz"




checkFillConfig :: FillConfig -> Maybe String
checkFillConfig FillConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, amountOfGaps} 
 | any (<0) [minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength,amountOfGaps] = Just "At least one of your integer parameters is negative."
 | null usedLiterals = Just "You did not specify which literals should be used."
 | minClauseAmount > maxClauseAmount = Just "The minimum amount of clauses is greater than the maximum amount."
 | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum."
 | lengthLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
 | amountOfGaps >  2^lengthLiterals = Just "There's not enough literals for this amount of gaps."
 | amountOfGaps > 2^(maxClauseAmount*maxClauseLength) = Just "This amount of gaps is not possible with your Clause length and amount settings."
 | otherwise = Nothing
  where lengthLiterals = length usedLiterals

checkCnfConfig :: CnfConfig -> Maybe String
checkCnfConfig CnfConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals}
 | any (<0) [minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength] = Just "At least one of your integer parameters is negative."
 | null usedLiterals = Just "You did not specify which literals should be used."
 | minClauseAmount > maxClauseAmount = Just "The minimum amount of clauses is greater than the maximum amount."
 | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum."
 | length usedLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
 | otherwise = Nothing


checkPickConfig :: PickConfig -> Maybe String
checkPickConfig PickConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, amountOfOptions, pickCnf}
 | any (<0) [minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength,amountOfOptions] = Just "At least one of your integer parameters is negative."
 | null usedLiterals = Just "You did not specify which literals should be used."
 | minClauseAmount > maxClauseAmount = Just "The minimum amount of clauses is greater than the maximum amount."
 | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum."
 | length usedLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
 | otherwise = Nothing


checkDecideConfig :: DecideConfig -> Maybe String
checkDecideConfig DecideConfig {minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength, usedLiterals, amountOfChanges}
 | any (<0) [minClauseAmount, maxClauseAmount, minClauseLength, maxClauseLength,amountOfChanges] = Just "At least one of your integer parameters is negative."
 | null usedLiterals = Just "You did not specify which literals should be used."
 | minClauseAmount > maxClauseAmount = Just "The minimum amount of clauses is greater than the maximum amount."
 | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum."
 | lengthLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
 | amountOfChanges >  2^lengthLiterals = Just "The table does not have enough entries to support this samount of changes."
 | amountOfChanges > 2^(maxClauseAmount*maxClauseLength) = Just "This amount of changes is not possible with your Clause length and amount settings."
 | otherwise = Nothing
  where lengthLiterals = length usedLiterals



checkStepConfig :: StepConfig -> Maybe String
checkStepConfig StepConfig {minClauseLength, maxClauseLength, usedLiterals} 
 | any (<0) [minClauseLength, maxClauseLength] = Just "At least one of your integer parameters is negative."
 | null usedLiterals = Just "You did not specify which literals should be used."
 | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum." 
 | length usedLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
 | otherwise = Nothing



checkResolutionConfig :: ResolutionConfig -> Maybe String
checkResolutionConfig ResolutionConfig {minClauseLength, maxClauseLength, steps, usedLiterals}
 | any (<0) [minClauseLength, maxClauseLength,steps] = Just "At least one of your integer parameters is negative."
 | null usedLiterals = Just "You did not specify which literals should be used."
 | minClauseLength > maxClauseLength = Just "The minimum clause length is greater than the maximum." 
 | lengthLiterals < minClauseLength = Just "There's not enough literals to satisfy your minimum clause length."
 | maxClauseLength == 1 && steps > 1 = Just "More than one step using only length 1 clauses is not possible."
 | steps > 2* lengthLiterals = Just "This amount of steps is impossible with the given amount of literals."
 | otherwise = Nothing
  where lengthLiterals = length usedLiterals 



withRatio :: Int -> (CNF -> Bool)
withRatio percentage = 
 (\c -> let tableEntries = readEntries (getTable c) in 
         length (filter (==Just True) tableEntries) == (length tableEntries *percentage `div` 100))

ensureChecksAndExecute :: (a -> Maybe String) -> (a -> IO()) -> a -> IO()
ensureChecksAndExecute checker exercise config = case checker config of Just message -> putStrLn message
                                                                        Nothing      -> exercise config


writeExercises :: Int -> a -> (a -> IO (String,b)) -> IO()                                         
writeExercises amount config exercise = write 1

 where write current
        | current > amount = return ()
        | otherwise = do
                  (desc,_) <- exercise config
                  appendFile "exercisetest.txt" (show (current) ++"\n" ++ desc ++"\n")
                  write (current+1)
