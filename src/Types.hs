module Types (

)where

import Test.QuickCheck

data SynTree 
    = And {lefttree :: SynTree , righttree :: SynTree} 
    | Or {lefttree :: SynTree , righttree :: SynTree} 
    | Not {folltree :: SynTree}
    | 