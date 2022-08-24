{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.LegalCNF.Quiz  (
    generateLegalCNFInst,
    feedback,
) where

import Tasks.LegalCNF.Config (LegalCNFInst(..), LegalCNFConfig(..))
import Config(CnfConfig(..), BaseConfig(..))
import Test.QuickCheck (Gen, suchThat, choose, vectorOf, elements)
import Trees.Types (SynTree(..), BinOp(..))
import Tasks.LegalCNF.GenerateIllegal (genIllegalSynTree, )
import Tasks.LegalCNF.GenerateLegal (genSynTreeWithCnf)
import Trees.Print(simplestDisplay)
import Tasks.LegalProposition.Parsing (illegalPropositionStringParse)
import Data.Set (fromList)
import Auxiliary (listNoDuplicate)
import Data.List ( (\\) )
import Trees.Helpers(transferCnfToSyntree)
import Types(genCnf)

generateLegalCNFInst :: LegalCNFConfig -> Gen LegalCNFInst
generateLegalCNFInst lCConfig@LegalCNFConfig {cnfConfig = CnfConfig{baseConf = BaseConfig{..}, ..}, ..} = do
    serialsOfWrong <- vectorOf illegals (choose (1, formulas) )`suchThat` listNoDuplicate
    let serial1 = [1.. formulas] \\ serialsOfWrong
    serialsOfExternal <- vectorOf externalGenFormulas (elements serial1 )`suchThat` \list -> listNoDuplicate list
    let serial2 = serial1 \\ serialsOfExternal
    serialsOfJustOneClause <- vectorOf (if includeFormWithJustOneClause then 1 else 0) (elements serial2)
    let serial3 = serial2 \\ serialsOfJustOneClause
    serialsOfJustOneLiteralPerClause <- vectorOf (if includeFormWithJustOneLiteralPerClause then 1 else 0) (elements serial3)
    treeList <- genSynTreeList serialsOfWrong serialsOfExternal serialsOfJustOneClause serialsOfJustOneLiteralPerClause [1.. formulas] lCConfig `suchThat` \treeList -> let treeList' = map (fmap (const 'a')) treeList in listNoDuplicate (map simplestDisplay treeList')
    return $ LegalCNFInst {serialsOfWrong = fromList serialsOfWrong, formulaStrings = map simplestDisplay treeList}


genSynTreeList :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> LegalCNFConfig -> Gen [SynTree BinOp Char]
genSynTreeList serialsOfWrong serialsOfExternal serialsOfJustOneClause serialsOfJustOneLiteralPerClause formulasList lCConfig = do
    mapM (genSynTreeWithSerial serialsOfWrong serialsOfExternal serialsOfJustOneClause serialsOfJustOneLiteralPerClause lCConfig) formulasList

genSynTreeWithSerial :: [Int] -> [Int] -> [Int] -> [Int] -> LegalCNFConfig -> Int -> Gen (SynTree BinOp Char)
genSynTreeWithSerial serialsOfWrong serialsOfExternal serialsOfJustOneClause serialsOfJustOneLiteralPerClause LegalCNFConfig {cnfConfig = CnfConfig{baseConf = BaseConfig{..}, ..}, ..} serial
    | serial `elem` serialsOfWrong = genIllegalSynTree (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals `suchThat` checkSize maxStringSize minStringSize
    | serial `elem` serialsOfExternal = do
        cnf <- genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals `suchThat` \cnf -> checkSize maxStringSize minStringSize (transferCnfToSyntree cnf)
        return (transferCnfToSyntree cnf)
    | serial `elem` serialsOfJustOneClause = genSynTreeWithCnf (1, 1) (minClauseLength, maxClauseLength) usedLiterals `suchThat` checkSize maxStringSize minStringSize
    | serial `elem` serialsOfJustOneLiteralPerClause = genSynTreeWithCnf (minClauseAmount, maxClauseAmount) (1, 1) usedLiterals `suchThat` checkSize maxStringSize minStringSize
    | otherwise = genSynTreeWithCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals `suchThat` checkSize maxStringSize minStringSize

checkSize :: Int -> Int -> SynTree BinOp Char -> Bool
checkSize maxStringSize minStringSize synTree = let stringLength = length (simplestDisplay synTree) in stringLength <= maxStringSize && stringLength >= minStringSize

feedback :: LegalCNFInst -> String -> Bool
feedback LegalCNFInst {serialsOfWrong} input = illegalPropositionStringParse input == Right serialsOfWrong
