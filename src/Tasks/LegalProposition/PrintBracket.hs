module Tasks.LegalProposition.PrintBracket (
    bracketDisplay,
) where

import Test.QuickCheck (Gen, elements, frequency)

import Trees.Types (SynTree(..), BinOp(..), showOperator, showOperatorNot)
import Trees.Print (normalShow)

bracketDisplay :: SynTree BinOp Char -> Gen String
bracketDisplay (Binary oper a b) = allocateBracketToSubtree False a b (showOperator oper)
bracketDisplay (Leaf a)=  return ("("++ (a : ")"))
bracketDisplay (Not a) = do
    aFormula <- ifUsebracket True a
    return (showOperatorNot ++ aFormula)

ifUsebracket :: Bool -> SynTree BinOp Char -> Gen String
ifUsebracket usebracket (Leaf a) = if not usebracket then return [a] else return ['(', a, ')']
ifUsebracket usebracket synTree@(Not a) = let addPositions = notAndLeaves a in
    if not usebracket
    then return (normalShow synTree)
    else frequency [(1, return("(" ++ showOperatorNot ++ normalShow a ++ ")")), (addPositions, subTreebracket synTree)]
ifUsebracket usebracket synTree@(Binary _ _ _) =
    if not usebracket
    then return (normalShow synTree)
    else subTreebracket synTree

subTreebracket :: SynTree BinOp Char -> Gen String
subTreebracket (Binary oper a b) = allocateBracketToSubtree True a b (showOperator oper)
subTreebracket (Not a) = do
    left <- ifUsebracket True a
    return (showOperatorNot ++ left)
subTreebracket (Leaf _) = error "This will not happen but must be write"

allocateBracketToSubtree :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> Gen String
allocateBracketToSubtree notFirstLayer a b usedOperator = do
    ifUseBug <- elements [True, False]
    left <- ifUsebracket ifUseBug a
    right <- ifUsebracket (not ifUseBug) b
    if notFirstLayer
    then return ("(" ++ left ++ " " ++ usedOperator ++ " " ++ right ++ ")")
    else return (left ++ " " ++ usedOperator ++ " " ++ right)

notAndLeaves :: SynTree o c -> Int
notAndLeaves (Binary _ a b) = notAndLeaves a + notAndLeaves b
notAndLeaves (Leaf _) = 1
notAndLeaves (Not a) = 1 + notAndLeaves a
