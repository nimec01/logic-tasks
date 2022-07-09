module Config (
    SubInst(..),

) where

import Types ( SynTree)

data SubInst = SubInst
    {
        insofSyntree :: SynTree Char,
        allsubtree :: [SynTree Char]
    }