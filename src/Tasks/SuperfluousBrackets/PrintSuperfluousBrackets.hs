module Tasks.SuperfluousBrackets.PrintSuperfluousBrackets(
    superfluousBracketsDisplay,
    sameOperatorAdjacentSerial,
)where

import Test.QuickCheck (Gen, frequency, elements, choose)

import Trees.Types (SynTree (..), Op(..), showOperator)
import Trees.Helpers (treeNodes, numberAllNodes)

superfluousBracketsDisplay :: SynTree Op Char -> Integer -> Gen String
superfluousBracketsDisplay synTree brackets =
    let synTreeWithSerial = numberAllNodes synTree
        serialsOfsameOperator = sameOperatorAdjacentSerial synTreeWithSerial Nothing
    in  do
        serial <- elements serialsOfsameOperator
        rootDisplay synTreeWithSerial (brackets -1) serial

rootDisplay :: SynTree (Op, Integer) (Char, Integer) -> Integer -> Integer -> Gen String
rootDisplay (Leaf _) _ _ =  error "can not have only one node"
rootDisplay synTree@(Unary (neg,_) a) brackets serial = do
    ifUsebrackets <- frequency [(fromIntegral brackets, return True), (fromIntegral (treeNodes synTree - brackets), return False)]
    if ifUsebrackets
    then do
        formula <- nonRootDisplay a (brackets - 1) neg (Just serial)
        return ("(" ++ showOperator neg ++ formula ++ ")")
    else do
        formula <- nonRootDisplay a brackets neg (Just serial)
        return (showOperator neg ++ formula)
rootDisplay (Binary operWithSerial a b) brackets serial = allocateBracketToSubtree a b operWithSerial brackets Nothing (Just serial)

allocateBracketToSubtree :: SynTree (Op, Integer) (Char, Integer) -> SynTree (Op, Integer) (Char, Integer) -> (Op, Integer) -> Integer -> Maybe Op -> Maybe Integer -> Gen String
allocateBracketToSubtree a b (oper, nowSerial) brackets fatherOperator serial
    | Just nowSerial == serial = do
        formula <- allocateBracketToSubtree a b (oper, nowSerial) brackets fatherOperator Nothing
        return ("(" ++ formula ++ ")")
    | otherwise = let rightNodes = fromIntegral (treeNodes b)
                      leftNodes = fromIntegral (treeNodes a)
                in  do
                ifUsebrackets <- frequency [(fromIntegral brackets, return True), (fromIntegral (rightNodes + leftNodes + 1 - brackets), return False)]
                let brackets' = if ifUsebrackets then brackets - 1 else brackets
                    addBracket = (if ifUsebrackets then 1 else 0) + (if nowSerial == 1 || (fatherOperator == Just oper && (oper == And || oper == Or)) then 0 else 1)
                leftBrackets <- choose (max 0 (brackets' - rightNodes) , min leftNodes brackets')
                leftFormula <- nonRootDisplay a leftBrackets oper serial
                rightFormula <- nonRootDisplay b (brackets' - leftBrackets) oper serial
                return (replicate addBracket '(' ++ leftFormula ++ showOperator oper ++ rightFormula ++ replicate addBracket ')')

nonRootDisplay :: SynTree (Op, Integer) (Char, Integer) -> Integer -> Op -> Maybe Integer -> Gen String  -- string is fatheroper
nonRootDisplay (Leaf (a, _)) brackets _ _ = if brackets == 0 then return [a] else return ("("++ (a : ")"))
nonRootDisplay synTree@(Unary (neg,_) a) brackets _ serial = do
    ifUsebrackets <- frequency [(fromIntegral brackets, return True), (fromIntegral (treeNodes synTree - brackets), return False)]
    if ifUsebrackets
    then do
        formula <- nonRootDisplay a (brackets - 1) neg serial
        return ("(" ++ showOperator neg ++ formula ++ ")")
    else do
        formula <- nonRootDisplay a brackets neg serial
        return (showOperator neg ++ formula)
nonRootDisplay (Binary operWithSerial a b) brackets fatherOperator serial = allocateBracketToSubtree a b operWithSerial brackets (Just fatherOperator) serial

sameOperatorAdjacentSerial :: SynTree (Op, Integer) (Char, Integer) -> Maybe Op -> [Integer]
sameOperatorAdjacentSerial (Leaf _) _ = []
sameOperatorAdjacentSerial (Unary (Not, _) a) _ = sameOperatorAdjacentSerial a (Just Not)
sameOperatorAdjacentSerial (Binary (oper, serial) a b) fatherOper
    | Just oper == fatherOper && (oper == And || oper == Or) = serial : sameOperatorAdjacentSerial a (Just oper) ++ sameOperatorAdjacentSerial b (Just oper)
    | otherwise = sameOperatorAdjacentSerial a (Just oper) ++ sameOperatorAdjacentSerial b (Just oper)
sameOperatorAdjacentSerial  _ _ = error "All cases handled!"
