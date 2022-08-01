module Tasks.LegalProposition.PrintIllegal (
    illegalDisplay,
) where

import Test.QuickCheck (Gen, frequency, elements, oneof)

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

illegalShow :: Bool -> SynTree Char -> SynTree Char -> String -> Gen String
illegalShow notFirstLayer a b usedLiterals =
    if notFirstLayer
    then  do
        letter <- elements usedLiterals
        frequency [(2, return ("(" ++ normalShow a ++ normalShow b ++ ")")), (1, return ("(" ++ normalShow a ++ "~" ++ normalShow b ++ ")")), (1, return ("(" ++ normalShow a ++ [letter] ++ normalShow b ++ ")"))]
    else  do
        letter <- elements usedLiterals
        frequency [(2, return (normalShow a ++ normalShow b)), (1,return (normalShow a ++ "~" ++ normalShow b)), (1, return (normalShow a ++ [letter] ++ normalShow b))]

implementIllegal :: Bool -> SynTree Char -> String -> Gen String
implementIllegal notFirstLayer (And a b) usedLiterals = illegalShow notFirstLayer a b usedLiterals
implementIllegal notFirstLayer (Or a b) usedLiterals = illegalShow notFirstLayer a b usedLiterals
implementIllegal notFirstLayer (Equi a b) usedLiterals = illegalShow notFirstLayer a b usedLiterals
implementIllegal notFirstLayer (Impl a b) usedLiterals = illegalShow notFirstLayer a b usedLiterals
implementIllegal _ (Not a) usedLiterals = do
    letter <- elements usedLiterals
    oneof $ map (return . (++ normalShow a)) ["/\\", "\\/", "=>", "<=>", [letter]]
implementIllegal _ (Leaf _) _ = do
    oper <- elements ["/\\", "\\/", "=>", "<=>", "~"]
    elements [oper,""]
