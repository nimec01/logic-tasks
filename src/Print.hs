module Print(
    transfer,
    main
    ) where
import Types
-- import Parsing
import Test.QuickCheck
import Data.Maybe

-- prop_syntree :: SynTree  -> Bool
-- prop_syntree a = normParse ( show a) == Right  a

-- prop_add :: Int ->Bool
-- prop_add a =(a+1)==(1+a)
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