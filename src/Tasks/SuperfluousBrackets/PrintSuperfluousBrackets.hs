module Tasks.SuperfluousBrackets.PrintSuperfluousBrackets(
    superfluousBracketsDisplay,
    sameAssociativeOperatorAdjacentSerial,
)where

import Test.QuickCheck (Gen, frequency, elements, choose)

import Trees.Types (SynTree (..), Op(..), showOperator)
import Trees.Helpers (treeNodes, numberAllNodes)

superfluousBracketsDisplay :: SynTree Op Char -> Integer -> Gen String
superfluousBracketsDisplay synTree brackets =
    let synTreeWithSerial = numberAllNodes synTree
        serialsOfsameOperator = sameAssociativeOperatorAdjacentSerial synTreeWithSerial Nothing
    in  do
        serial <- elements serialsOfsameOperator
        rootDisplay synTreeWithSerial (brackets -1) serial

rootDisplay :: SynTree (Op, Integer) Char -> Integer -> Integer -> Gen String
rootDisplay (Leaf _) _ _ =  error "can not have only one node"
rootDisplay synTree@(Unary (Not,_) a) brackets serial = do
    ifUsebrackets <- frequency [(fromIntegral brackets, return True), (fromIntegral (treeNodes synTree - brackets), return False)]
    if ifUsebrackets
    then do
        formula <- nonRootDisplay a (brackets - 1) Not (Just serial)
        return ("(" ++ showOperator Not ++ formula ++ ")")
    else do
        formula <- nonRootDisplay a brackets Not (Just serial)
        return (showOperator Not ++ formula)
rootDisplay (Binary operWithSerial a b) brackets serial = allocateBracketToSubtree a b operWithSerial brackets False Nothing (Just serial)
rootDisplay _ _ _ = error "All cases handled!"

allocateBracketToSubtree :: SynTree (Op, Integer) Char -> SynTree (Op, Integer) Char -> (Op, Integer) -> Integer -> Bool -> Maybe Op -> Maybe Integer -> Gen String
allocateBracketToSubtree a b (oper, nowSerial) brackets hasFather fatherOperator serial
    | Just nowSerial == serial = do
        formula <- allocateBracketToSubtree a b (oper, error "never gonna need this") brackets hasFather fatherOperator Nothing
        return ("(" ++ formula ++ ")")
    | otherwise = let rightNodes =  treeNodes b
                      leftNodes =  treeNodes a
                in  do
                ifUsebrackets <- frequency [(fromIntegral brackets, return True), (fromIntegral (rightNodes + leftNodes + 1 - brackets), return False)]
                let brackets' = if ifUsebrackets then brackets - 1 else brackets
                    addBracket = (if ifUsebrackets then 1 else 0) + (if not hasFather || (fatherOperator == Just oper && (oper == And || oper == Or)) then 0 else 1)
                leftBrackets <- choose (max 0 (brackets' - rightNodes) , min leftNodes brackets')
                leftFormula <- nonRootDisplay a leftBrackets oper serial
                rightFormula <- nonRootDisplay b (brackets' - leftBrackets) oper serial
                return (replicate addBracket '(' ++ leftFormula ++ " " ++ showOperator oper ++ " " ++ rightFormula ++ replicate addBracket ')')

nonRootDisplay :: SynTree (Op, Integer) Char -> Integer -> Op -> Maybe Integer -> Gen String
nonRootDisplay (Leaf a) 0 _ _ = return [a]
nonRootDisplay (Leaf a) 1 _ _ = return ("("++ (a : ")"))
nonRootDisplay synTree@(Unary (Not,_) a) brackets _ serial = do
    ifUsebrackets <- frequency [(fromIntegral brackets, return True), (fromIntegral (treeNodes synTree - brackets), return False)]
    if ifUsebrackets
    then do
        formula <- nonRootDisplay a (brackets - 1) Not serial
        return ("(" ++ showOperator Not ++ formula ++ ")")
    else do
        formula <- nonRootDisplay a brackets Not serial
        return (showOperator Not ++ formula)
nonRootDisplay (Binary operWithSerial a b) brackets fatherOperator serial = allocateBracketToSubtree a b operWithSerial brackets True (Just fatherOperator) serial
nonRootDisplay _ _ _ _ = error "All cases handled!"

sameAssociativeOperatorAdjacentSerial :: SynTree (Op, Integer) c -> Maybe Op -> [Integer]
sameAssociativeOperatorAdjacentSerial (Leaf _) _ = []
sameAssociativeOperatorAdjacentSerial (Unary (Not, _) a) _ = sameAssociativeOperatorAdjacentSerial a (Just Not)
sameAssociativeOperatorAdjacentSerial (Binary (oper, serial) a b) fatherOper
    | Just oper == fatherOper && (oper == And || oper == Or) =
        serial : sameAssociativeOperatorAdjacentSerial a (Just oper) ++ sameAssociativeOperatorAdjacentSerial b (Just oper)
    | otherwise =
        sameAssociativeOperatorAdjacentSerial a (Just oper) ++ sameAssociativeOperatorAdjacentSerial b (Just oper)
sameAssociativeOperatorAdjacentSerial  _ _ = error "All cases handled!"
