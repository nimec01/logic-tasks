module Tasks.LegalProposition.PrintIllegal (
    illegalDisplay,
) where

import Test.QuickCheck (Gen, frequency, elements)

import Types (SynTree(..), treeNodes, collectLeaves)
import Print (normalShow)

illegalDisplay :: SynTree Char -> Gen String
illegalDisplay synTree =
    let usedLiterals = collectLeaves synTree
    in ifUseIllegal True False synTree usedLiterals

ifUseIllegal :: Bool -> Bool -> SynTree Char -> String -> Gen String
ifUseIllegal useBug notFirstLayer synTree usedLiterals =
    let nodeNum = treeNodes synTree
    in if not useBug
       then return (normalShow synTree)
       else frequency [(1, implementIllegal notFirstLayer synTree usedLiterals), (fromIntegral nodeNum - 1, subTreeIllegal notFirstLayer synTree usedLiterals)]

subTreeIllegal ::Bool -> SynTree Char -> String -> Gen String
subTreeIllegal notFirstLayer (And a b) usedLiterals = allocateBugToSubtree notFirstLayer a b usedLiterals "/\\"
subTreeIllegal notFirstLayer (Or a b) usedLiterals = allocateBugToSubtree notFirstLayer a b usedLiterals "\\/"
subTreeIllegal notFirstLayer (Equi a b) usedLiterals = allocateBugToSubtree notFirstLayer a b usedLiterals "<=>"
subTreeIllegal notFirstLayer (Impl a b) usedLiterals = allocateBugToSubtree notFirstLayer a b usedLiterals "=>"
subTreeIllegal _ (Not a) usedLiterals = do
    left <- ifUseIllegal True True a usedLiterals
    return ("~" ++ left)
subTreeIllegal _ (Leaf _) _ = error "This will not happen but must be write"

allocateBugToSubtree :: Bool -> SynTree Char -> SynTree Char -> String -> String -> Gen String
allocateBugToSubtree notFirstLayer a b usedLiterals usedOperator = do
    ifUseBug <- elements [True, False]
    left <- ifUseIllegal ifUseBug True a usedLiterals
    right <- ifUseIllegal (not ifUseBug) True b usedLiterals
    if notFirstLayer
    then return ("(" ++ left ++ usedOperator ++ right ++ ")")
    else return (left ++ usedOperator ++ right)

illegalShow :: Bool -> SynTree Char -> SynTree Char -> String -> String -> Gen String
illegalShow notFirstLayer a b usedLiterals usedOperator =
    if notFirstLayer
    then  do
        letter <- elements usedLiterals
        frequency (map (\(probability, replacedOperator) -> (probability, combineNormalShow a b replacedOperator True)) [(2, ""), (2, "~"), (2, [letter])] ++ illegalParentheses a b usedOperator)
    else  do
        letter <- elements usedLiterals
        frequency (map (\(probability, replacedOperator) -> (probability, combineNormalShow a b replacedOperator False)) [(2, ""), (1, "~"), (1, [letter])])

combineNormalShow :: SynTree Char -> SynTree Char -> String -> Bool -> Gen String
combineNormalShow a b replacedOperator False = return (normalShow a ++ replacedOperator ++ normalShow b)
combineNormalShow a b replacedOperator True = return ("(" ++ normalShow a ++ replacedOperator ++ normalShow b ++ ")")


implementIllegal :: Bool -> SynTree Char -> String -> Gen String
implementIllegal notFirstLayer (And a b) usedLiterals = illegalShow notFirstLayer a b usedLiterals "/\\"
implementIllegal notFirstLayer (Or a b) usedLiterals = illegalShow notFirstLayer a b usedLiterals "\\/"
implementIllegal notFirstLayer (Equi a b) usedLiterals = illegalShow notFirstLayer a b usedLiterals "<=>"
implementIllegal notFirstLayer (Impl a b) usedLiterals = illegalShow notFirstLayer a b usedLiterals "=>"
implementIllegal _ (Not a) usedLiterals = do
    letter <- elements usedLiterals
    elements  $ map (++ normalShow a) ["/\\", "\\/", "=>", "<=>", [letter]]
implementIllegal _ (Leaf _) _ = do
    oper <- elements ["/\\", "\\/", "=>", "<=>", "~"]
    elements [oper,""]

illegalParentheses :: SynTree Char -> SynTree Char -> String -> [(Int, Gen String)]
illegalParentheses  a b usedOperator = [(1, return (formulaStr ++ ")")),(1, return ("(" ++ formulaStr))]
    where formulaStr = normalShow a ++ usedOperator ++ normalShow b
