{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Types where




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