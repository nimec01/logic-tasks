{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Tasks.LegalCNF.Config(
    checkLegalCNFConfig,
    defaultLegalCNFConfig,
    LegalCNFInst(..),
    LegalCNFConfig(..),
    lengthBound
) where
import Data.Set (Set)
import Config(CnfConfig(..), BaseConfig(..), dCnfConf)
import Data.Char (isLetter)

data LegalCNFConfig =
  LegalCNFConfig
  {
      cnfConfig :: CnfConfig
    , formulas :: Int
    , externalGenFormulas :: Int
    , illegals :: Int
    , includeFormWithJustOneClause :: Bool
    , includeFormWithJustOneLiteralPerClause :: Bool
    , maxStringSize :: Int
    , minStringSize :: Int
    , allowArrowOperators :: Bool
  } deriving Show

defaultLegalCNFConfig :: LegalCNFConfig
defaultLegalCNFConfig =
  LegalCNFConfig
  {
    cnfConfig = dCnfConf
  , formulas = 4
  , externalGenFormulas = 1
  , illegals = 2
  , includeFormWithJustOneClause = False
  , includeFormWithJustOneLiteralPerClause = True
  , maxStringSize = 30
  , minStringSize = 4
  , allowArrowOperators = True
  }

checkLegalCNFConfig :: LegalCNFConfig -> Maybe String
checkLegalCNFConfig LegalCNFConfig{cnfConfig = CnfConfig {baseConf = BaseConfig{..}, ..}, ..}
    | not (all isLetter usedLiterals)
      = Just "Only letters are allowed as literals."
    | minClauseAmount < 1
      = Just "The number of Clauses must be positive"
    | minClauseLength < 1
      = Just "The number of Literals per clause must be positive"
    | maxClauseAmount < minClauseAmount
      = Just "maxClauses can not less than minClauseAmount"
    | maxClauseLength < minClauseLength
      = Just "maxClauseLength can not less than minClauseLength"
    | (maxClauseLength > 2 * length usedLiterals) || (externalGenFormulas > 0 && maxClauseLength > length usedLiterals)
      = Just "The Used Literal can not generate a Clause with maxClauseLength"
    | let limit beginNumber = product (take maxClauseLength (reverse [1 .. beginNumber :: Integer])),
      let literalLength = fromIntegral (length usedLiterals),
      fromIntegral maxClauseAmount > min 15 (limit (2 * literalLength))
    || externalGenFormulas > 0 && fromIntegral maxClauseAmount > min 15 (limit literalLength)
      = Just "The maxClauseAmount is too big and have the risk contain same Clauses in the CNF"
    | fromIntegral formulas > (fromIntegral (maxClauseLength - minClauseLength + 1) ^ (fromIntegral (maxClauseAmount - minClauseAmount + 1) :: Integer)) `div` (2 :: Integer) + 1
      = Just  "Formulas is too big and have risk to generate similar CNF"
    | maxClauseLength == 1 && maxClauseAmount == 1
      = Just "Atomic propositions have no illegal forms"
    | formulas < 1
      = Just "The number of formulas must be positive"
    | illegals < 0
      = Just "The number of illegals can not be negative"
    | externalGenFormulas < 0
      = Just "The number external generated formulas can not be negative"
    | formulas - illegals - externalGenFormulas <  (if includeFormWithJustOneClause then 1 else 0) + (if includeFormWithJustOneLiteralPerClause then 1 else 0)
      = Just "The formulas used to generate special formula is not enough"
    | externalGenFormulas > 0 && minClauseAmount > lengthBound minClauseLength (length usedLiterals) (minClauseLength, maxClauseLength)
      = Just "The minimum Number of ClauseAmount is too big and the external generator can not generate such CNF "
    | minStringSize <= 0
      = Just "Can not generate String with such minStringSize"
    | maxStringSize < minStringSize
      = Just "The maximum length of formula string can not less than the minimum length of formula string"
    | minStringSize < max 1 minClauseAmount * ((minClauseLength - 1) * 5 + 1)
      = Just "Can not generate String with such minimum length of formula string"
    | maxStringSize > maxClauseAmount * (maxClauseLength * 6 + 5)
      = Just "Can not generate String with such maximum length of formula string"
    | otherwise
      = Nothing

lengthBound :: Int -> Int -> (Int, Int) -> Int --This function is in the logic Type when generate cnf
lengthBound 1 len (_,_)= 2*len
lengthBound n len (maxLen,minLen)
    | n == maxLen && n == minLen = 2^n
    | n == minLen = 2^n * len
    | n == len = 2^n + lengthBound (n-1) len (maxLen,minLen)
    | otherwise = 2^n * len + lengthBound (n-1) len (maxLen,minLen)

data LegalCNFInst =
    LegalCNFInst
    {
        serialsOfWrong :: Set Int
      , formulaStrings :: [String]
    } deriving Show
