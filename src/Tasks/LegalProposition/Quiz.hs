{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.LegalProposition.Quiz (
    genLegalPropositionInst,
    feedback,
) where

import Test.QuickCheck (Gen, choose, vectorOf, suchThat, generate)
import Data.List (sort)
import Data.List.Extra (nubOrd)

import Tasks.LegalProposition.Config (LegalPropositionConfig (..), LegalPropositionInst (..))
import Tasks.SynTree.Config (SynTreeConfig (..))
import Tasks.LegalProposition.PrintIllegal (illegalDisplay )
import Generate (genSynTree)
import Types ( SynTree )
import Print ( display, )
import Parsing (illegalPropositionStringParse)

genLegalPropositionInst :: LegalPropositionConfig -> IO LegalPropositionInst
genLegalPropositionInst lPConfig = generate (generateLegalPropositionInst lPConfig)

generateLegalPropositionInst :: LegalPropositionConfig -> Gen LegalPropositionInst
generateLegalPropositionInst LegalPropositionConfig  {formulaConfig = SynTreeConfig {..}, ..} = do
    treeList <- vectorOf (fromIntegral formulaNum) (genSynTree (minNode, maxNode) maxDepth usedLiterals atLeastOccurring useImplEqui)
    serialNumOfWrong <- vectorOf (fromIntegral illegalNum) (choose (1, fromIntegral formulaNum) )`suchThat` (\list -> nubOrd list ==list)
    pseudoFormulas <- genPseudoList 1 serialNumOfWrong treeList
    return $ LegalPropositionInst
        { serialNumOfWrong = sort serialNumOfWrong
        , pseudoFormulas = pseudoFormulas
        }

genPseudoList :: Int -> [Int] -> [SynTree Char] -> Gen [String]
genPseudoList _ _ [] = return []
genPseudoList point serialNumOfWrong (tree : treeList) =
    if point `elem` serialNumOfWrong
    then do
        str <- illegalDisplay tree
        rest <- genPseudoList (point + 1) serialNumOfWrong treeList
        return (str : rest)
    else do
        str <- legalDisplay tree
        rest <- genPseudoList (point + 1) serialNumOfWrong treeList
        return (str : rest)

legalDisplay :: SynTree Char -> Gen String
legalDisplay syntaxTree = return (display syntaxTree)

feedback :: LegalPropositionInst -> String -> Bool
feedback  LegalPropositionInst {serialNumOfWrong}  input = illegalPropositionStringParse input == Right serialNumOfWrong
