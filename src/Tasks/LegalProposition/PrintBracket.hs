module Tasks.LegalProposition.PrintBracket (
    bracketDisplay,
) where

import Test.QuickCheck (Gen, elements, frequency)

import Trees.Types (SynTree(..), Op(..), showOperator)
import Trees.Print (normalShow)

bracketDisplay :: SynTree Op Char -> Gen String
bracketDisplay (Binary oper a b) = allocateBracketToSubtree False a b (showOperator oper)
bracketDisplay (Leaf a)=  return ("("++ (a : ")"))
bracketDisplay (Unary Not a) = do
    aFormula <- ifUsebracket True a
    return (showOperator Not ++ aFormula)
bracketDisplay _ = error "All cases handled!"

ifUsebracket :: Bool -> SynTree Op Char -> Gen String
ifUsebracket usebracket (Leaf a) = if not usebracket then return [a] else return ['(', a, ')']
ifUsebracket usebracket synTree@(Unary Not a) = let addPositions = notAndLeaves a in
    if not usebracket
    then return (normalShow synTree)
    else frequency [(1, return("(" ++ showOperator Not ++ normalShow a ++ ")")), (addPositions, subTreebracket synTree)]
ifUsebracket usebracket synTree@(Binary _ _ _) =
    if not usebracket
    then return (normalShow synTree)
    else subTreebracket synTree
ifUsebracket _ _ = error "All cases handled!"

subTreebracket :: SynTree Op Char -> Gen String
subTreebracket (Binary oper a b) = allocateBracketToSubtree True a b (showOperator oper)
subTreebracket (Unary Not a) = do
    left <- ifUsebracket True a
    return (showOperator Not ++ left)
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
notAndLeaves (Binary _ a b) = notAndLeaves a + notAndLeaves b
notAndLeaves (Leaf _) =  1
notAndLeaves (Unary Not a) = 1 + notAndLeaves a
notAndLeaves _ = error "All cases handled!"
