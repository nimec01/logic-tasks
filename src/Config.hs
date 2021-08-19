{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

module Config where


import Data.Typeable
import GHC.Generics
import Types
import Formula



newtype Number = Number {value :: Maybe Int} deriving (Typeable, Generic)



data PickInst = PickInst {
                 cnfs    :: ! [Cnf]
               , correct :: ! Int
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)

dPickInst :: PickInst
dPickInst =  PickInst
          { cnfs = [mkCnf [mkClause [Literal 'A', Not 'B']], mkCnf [mkClause [Not 'A', Literal 'B']]]
          , correct = 1
          , addText = Just "Put additional text here or delete this parameter."
          }



data GiveInst = GiveInst {
                 cnf     :: ! Cnf
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)

dGiveInst :: GiveInst
dGiveInst =  GiveInst
          { cnf = mkCnf [mkClause [Literal 'A', Not 'B']]
          , addText = Just "Put additional text here or delete this parameter."
          }



data FillInst = FillInst {
                 cnf     :: ! Cnf
               , missing :: ! [Int]
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)

dFillInst :: FillInst
dFillInst =  FillInst
          { cnf = mkCnf [mkClause [Literal 'A', Not 'B']]
          , missing = [1,4]
          , addText = Just "Put additional text here or delete this parameter."
          }



data DecideInst = DecideInst {
                 cnf     :: ! Cnf
               , changed :: ! [Int]
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)

dDecideInst :: DecideInst
dDecideInst =  DecideInst
          { cnf = mkCnf [mkClause [Literal 'A', Not 'B']]
          , changed = [1,4]
          , addText = Just "Put additional text here or delete this parameter."
          }



data StepInst = StepInst {
                 clause1 :: ! Clause
               , clause2 :: ! Clause
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)

dStepInst :: StepInst
dStepInst =  StepInst
          { clause1 = mkClause [Not 'A', Not 'C', Literal 'B']
          , clause2 = mkClause [Literal 'A', Not 'C']
          , addText = Just "Put additional text here or delete this parameter."
          }



data ResolutionInst = ResolutionInst {
                 clauses :: ! [Clause]
               , addText    :: ! (Maybe String)
               }
               deriving (Typeable, Generic)

dResInst :: ResolutionInst
dResInst =  ResolutionInst
          { clauses = [mkClause [Not 'A', Not 'C', Literal 'B'], mkClause [Literal 'A', Not 'C'], mkClause [Literal 'C'], mkClause [Not 'B']]
          , addText = Just "Put additional text here or delete this parameter."
          }



data BaseConfig = BaseConfig
    { minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: String
    } deriving (Typeable, Generic)

dBaseConf :: BaseConfig
dBaseConf = BaseConfig {
      minClauseLength = 1
    , maxClauseLength = 3
    , usedLiterals = "ABCD"
    }



data CnfConfig = CnfConfig
    { baseConf:: BaseConfig
    , minClauseAmount :: Int
    , maxClauseAmount :: Int
    } deriving (Typeable, Generic)

dCnfConf :: CnfConfig
dCnfConf = CnfConfig
    { baseConf = dBaseConf
    , minClauseAmount = 2
    , maxClauseAmount = 3
    }



data PickConfig = PickConfig {
       cnfConf :: CnfConfig
     , amountOfOptions :: Int
     , pickCnf :: Bool
     , extraText :: Maybe String
     }
     deriving (Typeable, Generic)

dPickConf :: PickConfig
dPickConf = PickConfig
    { cnfConf = dCnfConf
    , amountOfOptions = 3
    , pickCnf = False
    , extraText = Nothing
    }



data FillConfig = FillConfig {
      cnfConf :: CnfConfig
    , percentageOfGaps :: Int
    , percentTrueEntries :: Maybe (Int,Int)
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)

dFillConf :: FillConfig
dFillConf = FillConfig
    { cnfConf = dCnfConf
    , percentageOfGaps = 40
    , percentTrueEntries = Just (30,70)
    , extraText = Nothing
    }



data GiveConfig = GiveConfig {
      cnfConf :: CnfConfig
    , percentTrueEntries :: Maybe (Int,Int)
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)

dGiveConf :: GiveConfig
dGiveConf = GiveConfig
    { cnfConf = dCnfConf
    , percentTrueEntries = Just (50,70)
    , extraText = Nothing
    }



data DecideConfig = DecideConfig {
      cnfConf :: CnfConfig
    , percentageOfChanged :: Int
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)

dDecideConf :: DecideConfig
dDecideConf = DecideConfig
    { cnfConf = dCnfConf
    , percentageOfChanged = 40
    , extraText = Nothing
    }



data StepConfig = StepConfig {
      baseConf :: BaseConfig
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)

dStepConf :: StepConfig
dStepConf = StepConfig
    { baseConf = dBaseConf
    , extraText = Nothing
    }



data ResolutionConfig = ResolutionConfig {
      baseConf :: BaseConfig
    , minSteps :: Int
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)

dResConf :: ResolutionConfig
dResConf = ResolutionConfig
    { baseConf = dBaseConf
    , minSteps = 2
    , extraText = Nothing
    }
