module Tasks.LegalProposition.PrintBracket (
    bracketDisplay,
) where

import Test.QuickCheck (Gen, frequency, elements)

import Types (SynTree(..), treeNodes)
import Print (normalShow)

bracketDisplay :: SynTree Char -> Gen String
bracketDisplay (And a b) = allocateBracketToSubtree False a b "/\\"
bracketDisplay (Leaf a)=  return ("("++ (a : ")"))
bracketDisplay (Or a b) = allocateBracketToSubtree False a b "\\/"
bracketDisplay (Not a) = do
    aFormula <- ifUsebracket True a
    return ('~' : aFormula)
bracketDisplay (Equi a b) = allocateBracketToSubtree False a b "<=>"
bracketDisplay (Impl a b) = allocateBracketToSubtree False a b "=>"

ifUsebracket :: Bool -> SynTree Char -> Gen String
ifUsebracket usebracket synTree =
    let nodeNum = treeNodes synTree
    in if not usebracket
       then return (normalShow synTree)
       else frequency [(1, implementBracket True synTree), (fromIntegral nodeNum - 1, subTreebracket synTree)]

subTreebracket :: SynTree Char -> Gen String
subTreebracket (And a b) = allocateBracketToSubtree True a b "/\\"
subTreebracket (Or a b) = allocateBracketToSubtree True a b "\\/"
subTreebracket (Equi a b) = allocateBracketToSubtree True a b "<=>"
subTreebracket (Impl a b) = allocateBracketToSubtree True a b "=>"
subTreebracket (Not a) = do
    left <- ifUsebracket True a
    return ("~" ++ left)
subTreebracket (Leaf _) = error "This will not happen but must be write"

allocateBracketToSubtree :: Bool -> SynTree Char -> SynTree Char -> String -> Gen String
allocateBracketToSubtree notFirstLayer a b usedOperator = do
    ifUseBug <- elements [True, False]
    left <- ifUsebracket ifUseBug a
    right <- ifUsebracket (not ifUseBug) b
    if notFirstLayer
    then return ("(" ++ left ++ usedOperator ++ right ++ ")")
    else return (left ++ usedOperator ++ right)

bracketShow :: Bool -> SynTree Char -> SynTree Char -> String -> Gen String
bracketShow notFirstLayer a b usedOperator =
    if notFirstLayer
    then  return ("((" ++ normalShow a ++ usedOperator ++ normalShow b ++ "))")
    else  return ("(" ++ normalShow a ++ usedOperator ++ normalShow b ++ ")")
        

implementBracket :: Bool -> SynTree Char -> Gen String
implementBracket notFirstLayer (And a b) = bracketShow notFirstLayer a b "/\\"
implementBracket notFirstLayer (Or a b) = bracketShow notFirstLayer a b "\\/"
implementBracket notFirstLayer (Equi a b) = bracketShow notFirstLayer a b "<=>"
implementBracket notFirstLayer (Impl a b) = bracketShow notFirstLayer a b "=>"
implementBracket _ (Not a) = return ("~(" ++ normalShow a ++ ")")
implementBracket _ (Leaf a) = return ("(" ++ [a] ++ ")")
