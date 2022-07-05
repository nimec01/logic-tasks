module Print(
    transfer,
    display
    ) where
import Types ( SynTree(Equi, And, Leaf, Or, Not, Impl) )


transfer :: SynTree ->String
transfer (And a b) = "[ $\\wedge $ " ++ transfer a++ transfer b++"  ]"
transfer (Leaf a)= "[" ++ (a:"]")
transfer (Or a b) = "[ $\\vee   $ " ++ transfer a++ transfer b++"  ]"
transfer (Not a) = "[ $\\neg $ " ++ transfer a ++"  ]"
transfer (Impl a b) = "[ $\\to  $ " ++ transfer a++ transfer b++"  ]"
transfer (Equi a b) = "[ $\\Leftrightarrow  $ " ++ transfer a++ transfer b++"  ]"


display :: SynTree-> String
display (And a b) = normalshow a ++"/\\"++ normalshow b
display (Leaf a)=  a:""
display (Or a b) = normalshow a ++"\\/"++ normalshow b
display (Not a) = "~" ++ normalshow a ++""
display (Impl a b) = normalshow a ++"=>"++ normalshow b
display (Equi a b) = normalshow a ++"<=>"++ normalshow b

normalshow:: SynTree-> String
normalshow (And a b) = "(" ++ normalshow a ++"/\\"++ normalshow b++")"
normalshow (Leaf a)=  a:""
normalshow (Or a b) = "(" ++ normalshow a ++"\\/"++ normalshow b++")"
normalshow (Not a) = "~" ++ normalshow a ++""
normalshow (Impl a b) = "(" ++ normalshow a ++"=>"++ normalshow b ++")"
normalshow (Equi a b) = "(" ++ normalshow a ++"<=>"++ normalshow b ++")"