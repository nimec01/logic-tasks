module Tasks.LegalProposition.PrintBracket (
    bracketDisplay,
) where

import Test.QuickCheck (Gen, elements, frequency)

import Types (SynTree(..),)
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
ifUsebracket usebracket (Leaf a) = if not usebracket then return [a] else return ['(', a, ')']
ifUsebracket usebracket synTree@(Not a) = let addPositions = notAndLeaves a in
    if not usebracket
    then return (normalShow synTree)
    else frequency [(1, return("(~"++ normalShow a ++ ")")), (addPositions, subTreebracket synTree)]
ifUsebracket usebracket synTree =
    if not usebracket
    then return (normalShow synTree)
    else subTreebracket synTree

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


notAndLeaves :: SynTree c -> Int
notAndLeaves (And a b) = notAndLeaves a + notAndLeaves b
notAndLeaves (Leaf _) =  1
notAndLeaves (Or a b) =  notAndLeaves a + notAndLeaves b
notAndLeaves (Not a) = 1 + notAndLeaves a
notAndLeaves (Impl a b) = notAndLeaves a + notAndLeaves b
notAndLeaves (Equi a b) = notAndLeaves a + notAndLeaves b
