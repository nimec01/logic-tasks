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
       else frequency [(1, implementBracket synTree), (fromIntegral nodeNum - 1, subTreebracket synTree)]

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

bracketShow :: SynTree Char -> SynTree Char -> String -> Gen String
bracketShow a b usedOperator = return ("((" ++ normalShow a ++ usedOperator ++ normalShow b ++ "))")

implementBracket :: SynTree Char -> Gen String
implementBracket (And a b) = bracketShow a b "/\\"
implementBracket (Or a b) = bracketShow a b "\\/"
implementBracket (Equi a b) = bracketShow a b "<=>"
implementBracket (Impl a b) = bracketShow a b "=>"
implementBracket (Not a) = return ("(~" ++ normalShow a ++ ")")
implementBracket (Leaf a) = return ("(" ++ [a] ++ ")")
