module Trees.Print (
  transferToPicture,
  display,
  normalShow,
  simplestDisplay,
  ) where

import Trees.Types (SynTree(..), Op(..), showOperator)

transferToPicture :: SynTree Op Char -> String
transferToPicture (Binary And a b) = "[ $\\wedge $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Leaf a) = "[" ++ (a:"]")
transferToPicture (Binary Or a b) = "[ $\\vee   $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Unary Not a) = "[ $\\neg $ " ++ transferToPicture a ++ "  ]"
transferToPicture (Binary Impl a b) = "[ $\\to  $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Binary Equi a b) = "[ $\\Leftrightarrow  $ " ++ transferToPicture a ++ transferToPicture b ++"  ]"
transferToPicture _ = error "All cases handled!"

display :: SynTree Op Char -> String
display (Binary oper a b) = normalShow a ++ " " ++ showOperator oper ++ " " ++ normalShow b
display (Leaf a)=  a : ""
display (Unary Not a) = showOperator Not ++ normalShow a ++ ""
display _ = error "All cases handled!"

normalShow :: SynTree Op Char -> String
normalShow (Binary oper a b) = "(" ++ normalShow a ++ " " ++ showOperator oper ++ " " ++ normalShow b ++ ")"
normalShow (Leaf a)=  a : ""
normalShow (Unary Not a) = showOperator Not ++ normalShow a
normalShow _ = error "All cases handled!"

simplestDisplay :: SynTree Op Char -> String
simplestDisplay (Leaf a)=  a : ""
simplestDisplay (Unary Not a) = showOperator Not ++ simplestShow a Not
simplestDisplay (Binary oper a b) = simplestShow a oper ++ " " ++ showOperator oper ++ " " ++ simplestShow b oper
simplestDisplay _ = error "All cases handled!"

simplestShow :: SynTree Op Char -> Op -> String
simplestShow (Leaf a) _ =  a : ""
simplestShow (Unary Not a) _ = showOperator Not ++ simplestShow a Not
simplestShow (Binary And a b) And = simplestShow a And ++ " " ++ showOperator And ++ " " ++ simplestShow b And
simplestShow (Binary Or a b) Or = simplestShow a Or ++ " " ++ showOperator Or ++ " " ++ simplestShow b Or
simplestShow (Binary oper a b) _ = "(" ++ simplestShow a oper ++ " " ++ showOperator oper ++ " " ++ simplestShow b oper ++ ")"
simplestShow _ _ = error "All cases handled!"
