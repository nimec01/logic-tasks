module Print(
    transfer,
    main
    ) where
import Types
    ( genSynTree, SynTree(Equi, And, Leaf, Or, Not, Impl) )
import Test.QuickCheck ( generate )
import Data.Maybe ( fromJust )

transfer :: SynTree ->String
transfer (And a b) = "[ $\\wedge $ " ++ transfer a++ transfer b++"  ]"
transfer (Leaf a)= "[" ++ (a:"]")
transfer (Or a b) = "[ $\\vee   $ " ++ transfer a++ transfer b++"  ]"
transfer (Not a) = "[ $\\neg $ " ++ transfer a ++"  ]"
transfer (Impl a b) = "[ $\\to  $ " ++ transfer a++ transfer b++"  ]"
transfer (Equi a b) = "[ $\\Leftrightarrow  $ " ++ transfer a++ transfer b++"  ]"

main :: IO ()
main = do
 tree <- generate $ fromJust $genSynTree (4, 10) 5 "ABCD" ""
 putStrLn (transfer tree)