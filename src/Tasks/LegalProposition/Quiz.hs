{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.LegalProposition.Quiz (
    genLegalPropositionInst,
    generateLegalPropositionInst,
    feedback,
) where

import Test.QuickCheck (Gen, choose, vectorOf, suchThat, generate)
import Data.List.Extra (nubOrd)
import Data.Set (fromList)

import Tasks.LegalProposition.Config (LegalPropositionConfig (..), LegalPropositionInst (..))
import Tasks.SynTree.Config (SynTreeConfig (..))
import Tasks.LegalProposition.PrintIllegal (illegalDisplay )
import Tasks.LegalProposition.PrintBracket (bracketDisplay)
import Generate (genSynTree, similarExist)
import Types (SynTree, Op)
import Print ( display, )
import Parsing (illegalPropositionStringParse)
import Data.Char (isLetter)

genLegalPropositionInst :: LegalPropositionConfig -> IO LegalPropositionInst
genLegalPropositionInst lPConfig = generate (generateLegalPropositionInst lPConfig)

generateLegalPropositionInst :: LegalPropositionConfig -> Gen LegalPropositionInst
generateLegalPropositionInst LegalPropositionConfig  {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    treeList <- vectorOf (fromIntegral formulas) (genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring useImplEqui maxConsecutiveNegations) `suchThat` (not . similarExist)
    serialsOfWrong <- vectorOf (fromIntegral illegals) (choose (1, fromIntegral formulas) )`suchThat` (\list -> nubOrd list ==list)
    serialsOfBracket <- vectorOf (fromIntegral bracketFormulas) (choose (1, fromIntegral formulas) )`suchThat` (\list -> nubOrd (list ++ serialsOfWrong) ==list ++ serialsOfWrong)
    pseudoFormulas <- genPseudoList serialsOfWrong serialsOfBracket treeList `suchThat` noSimilarFormulas
    return $ LegalPropositionInst
        { serialsOfWrong = fromList serialsOfWrong
        , pseudoFormulas = pseudoFormulas
        }

genPseudoList :: [Int] -> [Int] -> [SynTree Op Char] -> Gen [String]
genPseudoList serialsOfWrong serialsOfBracket trees =
    let pointedTrees = zip [1..] trees
    in
        mapM (\(point, tree) -> if point `elem` serialsOfWrong
            then illegalDisplay tree
            else if point `elem` serialsOfBracket
                then bracketDisplay tree
                else legalDisplay tree) pointedTrees

legalDisplay :: SynTree Op Char -> Gen String
legalDisplay syntaxTree = return (display syntaxTree)

feedback :: LegalPropositionInst -> String -> Bool
feedback  LegalPropositionInst {serialsOfWrong}  input = illegalPropositionStringParse input == Right serialsOfWrong

noSimilarFormulas :: [String] -> Bool
noSimilarFormulas pseudoFormulas = let pseudoFormulas' = map replace pseudoFormulas in  nubOrd pseudoFormulas' == pseudoFormulas'

replace :: String -> String
replace = map judgeAndChange

judgeAndChange :: Char -> Char
judgeAndChange symbol = if isLetter symbol then '_' else symbol
