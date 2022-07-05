{-# LANGUAGE DeriveGeneric #-}
module Config (
    SynTreeConfig(..),
    SynTreeInst(..) ,
    SubInst(..),
    dSynTreeConfig,
    dSubInst
) where
import Types ( SynTree(Leaf, Not), allsubtre )

data SynTreeConfig = SynTreeConfig
    {maxnode::Integer,
    minnode ::Integer,
    maxdepth :: Integer,
    electliteral::String,
    mustcontain ::String
    } deriving Show

dSynTreeConfig :: SynTreeConfig
dSynTreeConfig = SynTreeConfig {
    maxnode = 10,
    minnode = 6,
    maxdepth = 4,
    electliteral = "ABCDE",
    mustcontain = "ABC"

}

data SynTreeInst = SynTreeInst
    { insSyntree :: SynTree,
    image:: String,
    correct :: String
    } deriving(Show)

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