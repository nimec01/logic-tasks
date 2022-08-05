module Tasks.LegalProposition.PrintBracket (
    bracketDisplay,
) where

import Test.QuickCheck (Gen, elements, frequency)

import Types (SynTree(..), Op(..))
import Print (normalShow)

bracketDisplay :: SynTree Op Char -> Gen String
bracketDisplay (Binary And a b) = allocateBracketToSubtree False a b "/\\"
bracketDisplay (Leaf a)=  return ("("++ (a : ")"))
bracketDisplay (Binary Or a b) = allocateBracketToSubtree False a b "\\/"
bracketDisplay (Unary Not a) = do
    aFormula <- ifUsebracket True a
    return ('~' : aFormula)
bracketDisplay (Binary Equi a b) = allocateBracketToSubtree False a b "<=>"
bracketDisplay (Binary Impl a b) = allocateBracketToSubtree False a b "=>"
bracketDisplay _ = error "All cases handled!"

ifUsebracket :: Bool -> SynTree Op Char -> Gen String
ifUsebracket usebracket (Leaf a) = if not usebracket then return [a] else return ['(', a, ')']
ifUsebracket usebracket synTree@(Unary Not a) = let addPositions = notAndLeaves a in
    if not usebracket
    then return (normalShow synTree)
    else frequency [(1, return("(~"++ normalShow a ++ ")")), (addPositions, subTreebracket synTree)]
ifUsebracket usebracket synTree@(Binary _ _ _) =
    if not usebracket
    then return (normalShow synTree)
    else subTreebracket synTree
ifUsebracket _ _ = error "All cases handled!"

subTreebracket :: SynTree Op Char -> Gen String
subTreebracket (Binary And a b) = allocateBracketToSubtree True a b "/\\"
subTreebracket (Binary Or a b) = allocateBracketToSubtree True a b "\\/"
subTreebracket (Binary Equi a b) = allocateBracketToSubtree True a b "<=>"
subTreebracket (Binary Impl a b) = allocateBracketToSubtree True a b "=>"
subTreebracket (Unary Not a) = do
    left <- ifUsebracket True a
    return ("~" ++ left)
subTreebracket (Leaf _) = error "This will not happen but must be write"
subTreebracket _ = error "All cases handled!"

allocateBracketToSubtree :: Bool -> SynTree Op Char -> SynTree Op Char -> String -> Gen String
allocateBracketToSubtree notFirstLayer a b usedOperator = do
    ifUseBug <- elements [True, False]
    left <- ifUsebracket ifUseBug a
    right <- ifUsebracket (not ifUseBug) b
    if notFirstLayer
    then return ("(" ++ left ++ usedOperator ++ right ++ ")")
    else return (left ++ usedOperator ++ right)


notAndLeaves :: SynTree Op c -> Int
notAndLeaves (Binary And a b) = notAndLeaves a + notAndLeaves b
notAndLeaves (Leaf _) =  1
notAndLeaves (Binary Or a b) =  notAndLeaves a + notAndLeaves b
notAndLeaves (Unary Not a) = 1 + notAndLeaves a
notAndLeaves (Binary Impl a b) = notAndLeaves a + notAndLeaves b
notAndLeaves (Binary Equi a b) = notAndLeaves a + notAndLeaves b
notAndLeaves _ = error "All cases handled!"
