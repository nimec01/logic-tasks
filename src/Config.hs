{-# LANGUAGE DeriveGeneric #-}
module Config (
    SynTreeConfig(..),
    PickInst(..) ,
    SubInst(..),
    dSynTreeConfig,
    dPickInst,
    dSubInst
) where
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Types ( SynTree(Leaf, Not), allsubtre )
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

data SubInst = SubInst
    {
        insofSyntree::SynTree,
        allsubtree:: [SynTree]
    }

dSubInst ::SubInst
dSubInst = SubInst
    {
        insofSyntree = Not $ Leaf 'A',
        allsubtree =  allsubtre $ Not $ Leaf 'A'
    }