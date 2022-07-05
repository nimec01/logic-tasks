module Config (
    SubInst(..),
    dSubInst
) where

import Types ( SynTree(Leaf, Not), allsubtre )

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
