module Tasks.LegalProposition.PrintIllegal (
    illegalDisplay,
) where

import Test.QuickCheck (Gen, frequency, elements, oneof)

import Types (SynTree(..), treeNodes, collectLeaves)
import Print (normalShow)

-- collectleaves
illegalDisplay :: SynTree Char -> Gen String
illegalDisplay synTree =
    let usedStr = collectLeaves synTree
    in ifUseIllegal True False synTree usedStr

ifUseIllegal :: Bool -> Bool -> SynTree Char -> String -> Gen String
ifUseIllegal useBug notFirstLayer synTree usedStr =
    let nodeNum = treeNodes synTree
    in if not useBug
       then return (normalShow synTree)
       else frequency [(1, implementIllegal synTree notFirstLayer usedStr), (fromIntegral nodeNum - 1, subTreeIllegal notFirstLayer synTree usedStr)] --  加层数判定

subTreeIllegal ::Bool -> SynTree Char -> String -> Gen String
subTreeIllegal notFirstLayer (And a b) usedStr = allocateBugToSubtree notFirstLayer "/\\" a b usedStr
subTreeIllegal notFirstLayer (Or a b) usedStr = allocateBugToSubtree notFirstLayer "\\/" a b usedStr
subTreeIllegal notFirstLayer (Equi a b) usedStr = allocateBugToSubtree notFirstLayer "<=>" a b usedStr
subTreeIllegal notFirstLayer (Impl a b) usedStr = allocateBugToSubtree notFirstLayer "=>" a b usedStr
subTreeIllegal _ (Not a) usedStr = do
    left <- ifUseIllegal True True a usedStr
    return ("~" ++ left)
subTreeIllegal _ (Leaf _) _ = return ""

allocateBugToSubtree :: Bool -> String -> SynTree Char -> SynTree Char -> String -> Gen String
allocateBugToSubtree notFirstLayer op a b usedStr = do
    ifUseBug <- elements [True, False]
    left <- ifUseIllegal ifUseBug True a usedStr
    right <- ifUseIllegal (not ifUseBug) True b usedStr
    if notFirstLayer
    then return ("(" ++ left ++ op ++ right ++ ")")
    else return (left ++ op ++ right)

illegalShow :: Bool -> SynTree Char -> SynTree Char -> String -> String -> Gen String
illegalShow notFirstLayer a b usedStr usedOperator =
    if notFirstLayer
    then  do
        letter <- elements usedStr
        oneof [return ("(" ++ normalShow a ++ normalShow b ++ ")"), oneof [return ("(" ++ normalShow a ++ "~" ++ normalShow b ++ ")"), return ("(" ++ normalShow a ++ [letter] ++ normalShow b ++ ")")], oneof (illegalParentheses a b usedOperator)]
    else  do
        letter <- elements usedStr
        oneof [return (normalShow a ++ normalShow b), oneof [return (normalShow a ++ "~" ++ normalShow b), return (normalShow a ++ [letter] ++ normalShow b)]]

implementIllegal :: SynTree Char -> Bool -> String -> Gen String
implementIllegal (And a b) notFirstLayer usedStr = illegalShow notFirstLayer a b usedStr "/\\"
implementIllegal (Or a b) notFirstLayer usedStr = illegalShow notFirstLayer a b usedStr "\\/"
implementIllegal (Equi a b) notFirstLayer usedStr = illegalShow notFirstLayer a b usedStr "<=>"
implementIllegal (Impl a b) notFirstLayer usedStr = illegalShow notFirstLayer a b usedStr "=>"
implementIllegal (Not a) _ usedStr = do
    letter <- elements usedStr
    oneof $ map (return . (++ normalShow a)) ["/\\", "\\/", "=>", "<=>", [letter]]
implementIllegal (Leaf _) _ _ = do
    oper <- elements ["/\\", "\\/", "=>", "<=>", "~"]
    oneof [return oper, return ""]

illegalParentheses :: SynTree Char -> SynTree Char -> String -> [Gen String]
illegalParentheses  a b usedOperator = [return (formulaStr ++ ")"), return ("(" ++ formulaStr), return (")" ++ formulaStr ++ "(")]
  where formulaStr = normalShow a ++ usedOperator ++ normalShow b
