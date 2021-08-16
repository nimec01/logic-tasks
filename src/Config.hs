{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

module Config where


import Data.Typeable
import GHC.Generics
import Types



newtype Number = Number {value :: Maybe Int} deriving (Typeable, Generic)



data PickInst = PickInst {
                 cnfs    :: ! [Cnf]
               , correct :: ! Int
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)



data GiveInst = GiveInst {
                 cnf     :: ! Cnf
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)





data FillInst = FillInst {
                 cnf     :: ! Cnf
               , missing :: ! [Int]
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)







data DecideInst = DecideInst {
                 cnf     :: ! Cnf
               , changed :: ! [Int]
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)



data StepInst = StepInst {
                 clause1 :: ! Clause
               , clause2 :: ! Clause
               , addText :: ! (Maybe String)
               }
               deriving (Typeable, Generic)



data ResolutionInst = ResolutionInst {
                 clauses :: ! [Clause]
               , addText    :: ! (Maybe String)
               }
               deriving (Typeable, Generic)





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


data PickConfig = PickConfig {
       cnfConf :: CnfConfig
     , amountOfOptions :: Int
     , pickCnf :: Bool
     , extraText :: Maybe String
     }
     deriving (Typeable, Generic)


dCnfConf :: CnfConfig
dCnfConf = CnfConfig
    { baseConf = dBaseConf
    , minClauseAmount = 2
    , maxClauseAmount = 3
    }



data FillConfig = FillConfig {
      cnfConf :: CnfConfig
    , percentageOfGaps :: Int
    , percentTrueEntries :: Maybe (Int,Int)
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)



data GiveConfig = GiveConfig {
      cnfConf :: CnfConfig
    , percentTrueEntries :: Maybe (Int,Int)
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)



data DecideConfig = DecideConfig {
      cnfConf :: CnfConfig
    , percentageOfChanged :: Int
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)




data StepConfig = StepConfig {
      baseConf :: BaseConfig
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)



data ResolutionConfig = ResolutionConfig {
      baseConf :: BaseConfig
    , minSteps :: Int
    , extraText :: Maybe String
    }
    deriving (Typeable, Generic)
