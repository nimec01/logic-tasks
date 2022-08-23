{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

module Config where


import Data.Typeable
import GHC.Generics

data BaseConfig = BaseConfig
    { minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: String
    } deriving (Typeable, Generic, Show)

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
    } deriving (Typeable, Generic, Show)

dCnfConf :: CnfConfig
dCnfConf = CnfConfig
    { baseConf = dBaseConf
    , minClauseAmount = 2
    , maxClauseAmount = 3
    }


