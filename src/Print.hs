module Print(
    transferToPicture,
    display,
    normalShow,
    displaySubtrees
    ) where

import Types (SynTree(..),)
import Data.List (intercalate)

transferToPicture :: SynTree Char -> String
transferToPicture (And a b) = "[ $\\wedge $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Leaf a) = "[" ++ (a:"]")
transferToPicture (Or a b) = "[ $\\vee   $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Not a) = "[ $\\neg $ " ++ transferToPicture a ++ "  ]"
transferToPicture (Impl a b) = "[ $\\to  $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Equi a b) = "[ $\\Leftrightarrow  $ " ++ transferToPicture a ++ transferToPicture b ++"  ]"

display :: SynTree Char -> String
display (And a b) = normalShow a ++ "/\\" ++ normalShow b
display (Leaf a)=  a : ""
display (Or a b) = normalShow a ++ "\\/" ++ normalShow b
display (Not a) = "~" ++ normalShow a ++ ""
display (Impl a b) = normalShow a ++ "=>" ++ normalShow b
display (Equi a b) = normalShow a ++ "<=>" ++ normalShow b

normalShow :: SynTree Char -> String
normalShow (And a b) = "(" ++ normalShow a ++ "/\\" ++ normalShow b ++ ")"
normalShow (Leaf a)=  a : ""
normalShow (Or a b) = "(" ++ normalShow a ++ "\\/" ++ normalShow b ++ ")"
normalShow (Not a) = "~" ++ normalShow a ++ ""
normalShow (Impl a b) = "(" ++ normalShow a ++ "=>" ++ normalShow b ++ ")"
normalShow (Equi a b) = "(" ++ normalShow a ++ "<=>" ++ normalShow b ++ ")"

displaySubtrees :: [SynTree Char] -> String
displaySubtrees trees = "{" ++ showTrees trees ++ "}"

showTrees :: [SynTree Char] -> String
showTrees synTreeList = intercalate ", " (map display synTreeList)
