module Print(
    transferToPicture,
    display,
    displaySubTrees,
    normalShow,
    ) where

import Types (SynTree(..), Op(..))
import Data.List (intercalate)

transferToPicture :: SynTree Op Char -> String
transferToPicture (Binary And a b) = "[ $\\wedge $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Leaf a) = "[" ++ (a:"]")
transferToPicture (Binary Or a b) = "[ $\\vee   $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Unary Not a) = "[ $\\neg $ " ++ transferToPicture a ++ "  ]"
transferToPicture (Binary Impl a b) = "[ $\\to  $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Binary Equi a b) = "[ $\\Leftrightarrow  $ " ++ transferToPicture a ++ transferToPicture b ++"  ]"
transferToPicture _ = error "All cases handled!"

display :: SynTree Op Char -> String
display (Binary And a b) = normalShow a ++ "/\\" ++ normalShow b
display (Leaf a)=  a : ""
display (Binary Or a b) = normalShow a ++ "\\/" ++ normalShow b
display (Unary Not a) = "~" ++ normalShow a ++ ""
display (Binary Impl a b) = normalShow a ++ "=>" ++ normalShow b
display (Binary Equi a b) = normalShow a ++ "<=>" ++ normalShow b
display _ = error "All cases handled!"

normalShow :: SynTree Op Char -> String
normalShow (Binary And a b) = "(" ++ normalShow a ++ "/\\" ++ normalShow b ++ ")"
normalShow (Leaf a)=  a : ""
normalShow (Binary Or a b) = "(" ++ normalShow a ++ "\\/" ++ normalShow b ++ ")"
normalShow (Unary Not a) = "~" ++ normalShow a
normalShow (Binary Impl a b) = "(" ++ normalShow a ++ "=>" ++ normalShow b ++ ")"
normalShow (Binary Equi a b) = "(" ++ normalShow a ++ "<=>" ++ normalShow b ++ ")"
normalShow _ = error "All cases handled!"

displaySubTrees :: [SynTree Op Char] -> String
displaySubTrees trees = "{" ++ showTrees trees ++ "}"

showTrees :: [SynTree Op Char] -> String
showTrees synTreeList = intercalate ", " (map display synTreeList)
