{-# LANGUAGE DeriveGeneric #-}
module Config (
    SynTreeConfig(..),
    PickInst(..) ,
    dSynTreeConfig,
    dPickInst
) where
import Data.Typeable
import GHC.Generics
import Types
import Print (transfer)

data SynTreeConfig = SynTreeConfig
    {maxnode::Integer,
    minnode ::Integer,
    maxdepth :: Integer,
    electliteral::String,
    mustcontain ::String
    }deriving (Typeable, Generic)

dSynTreeConfig :: SynTreeConfig
dSynTreeConfig = SynTreeConfig {
    maxnode = 10,
    minnode = 6,
    maxdepth = 4,
    electliteral = "ABCDE",
    mustcontain = "ABC"

}

data PickInst = PickInst
    { insSyntree :: SynTree,
    image:: String,
    correct :: String
    } deriving(Show)

dPickInst :: PickInst
dPickInst = PickInst{
    insSyntree = Not $ Leaf 'A',
    image = transfer $ insSyntree dPickInst,
    correct =show $ insSyntree dPickInst
    }