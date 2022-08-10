module Print(
    transferToPicture,
    display,
    displaySubTrees,
    normalShow,
    simplestDisplay,
    showOperator
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
display (Binary oper a b) = normalShow a ++ showOperator oper ++ normalShow b
display (Leaf a)=  a : ""
display (Unary Not a) = showOperator Not ++ normalShow a ++ ""
display _ = error "All cases handled!"

normalShow :: SynTree Op Char -> String
normalShow (Binary oper a b) = "(" ++ normalShow a ++ showOperator oper ++ normalShow b ++ ")"
normalShow (Leaf a)=  a : ""
normalShow (Unary Not a) = showOperator Not ++ normalShow a
normalShow _ = error "All cases handled!"

displaySubTrees :: [SynTree Op Char] -> String
displaySubTrees trees = "{" ++ showTrees trees ++ "}"

showTrees :: [SynTree Op Char] -> String
showTrees synTreeList = intercalate ", " (map display synTreeList)

simplestDisplay :: SynTree Op Char -> String
simplestDisplay (Leaf a)=  a : ""
simplestDisplay (Unary Not a) = showOperator Not ++ simplestShow a (Just Not)
simplestDisplay (Binary oper a b) = simplestShow a (Just oper) ++ showOperator oper ++ simplestShow b (Just oper)
simplestDisplay _ = error "All cases handled!"

simplestShow :: SynTree Op Char -> Maybe Op -> String
simplestShow (Leaf a) _ =  a : ""
simplestShow (Unary Not a) _ = showOperator Not ++ simplestShow a (Just Not)
simplestShow (Binary And a b) (Just And) = simplestShow a (Just And) ++ "/\\" ++ simplestShow b (Just And)
simplestShow (Binary Or a b) (Just Or) = simplestShow a (Just Or) ++ "\\/" ++ simplestShow b (Just Or)
simplestShow (Binary oper a b) _ = "(" ++ simplestShow a (Just oper) ++ showOperator oper ++ simplestShow b (Just oper) ++ ")"
simplestShow _ _ = error "All cases handled!"

showOperator :: Op -> String
showOperator And = "/\\"
showOperator Or = "\\/"
showOperator Impl = "=>"
showOperator Equi = "<=>"
showOperator Not = "~"
