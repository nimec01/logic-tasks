{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module FillSpec where

-- jscpd:ignore-start
import Test.Hspec
import Test.QuickCheck (forAll, Gen, choose, elements, suchThat, sublistOf)
import Control.OutputCapable.Blocks (LangM, Rated)
import Config (
  dFillConf,
  FillConfig (..),
  FillInst (..),
  FormulaConfig(..),
  BaseConfig(..),
  dBaseConf,
  NormalFormConfig(..),
  dNormalFormConf
 )
import LogicTasks.Semantics.Fill (verifyQuiz, genFillInst, verifyStatic, partialGrade, completeGrade, description)
import Data.Maybe (fromMaybe)
import SynTreeSpec (validBoundsSynTree)
import Formula.Types (Table(getEntries), getTable, lengthBound, TruthValue (TruthValue))
import Tasks.SynTree.Config (SynTreeConfig(..))
import Util (withRatio, checkBaseConf, checkNormalFormConfig)
import LogicTasks.Util (formulaDependsOnAllAtoms)
import TestHelpers (doesNotRefuse)
-- jscpd:ignore-end

validBoundsBase :: Gen BaseConfig
validBoundsBase = do
  minClauseLength <- choose (1, 5)
  maxClauseLength <- choose (2, 10) `suchThat` \x -> minClauseLength <= x
  usedAtoms <- sublistOf ['A' .. 'Z'] `suchThat` \xs -> length xs >= maxClauseLength
  pure $ BaseConfig {
    minClauseLength
  , maxClauseLength
  , usedAtoms
  }

validBoundsCnf :: Gen NormalFormConfig
validBoundsCnf = do
  minClauseAmount <- choose (1, 5)
  maxClauseAmount <- choose (2, 10) `suchThat` \x -> minClauseAmount <= x
  baseConf <- validBoundsBase `suchThat` \bc ->
    minClauseAmount * minClauseLength bc >= length (usedAtoms bc) &&
    minClauseAmount <= 2 ^ length (usedAtoms bc) &&
    minClauseAmount <= lengthBound (length (usedAtoms bc)) (maxClauseLength bc)
  pure $ NormalFormConfig {
    baseConf
  , minClauseAmount
  , maxClauseAmount
  }

validBoundsFill :: Gen FillConfig
validBoundsFill = do
  -- formulaType <- elements ["Cnf", "Dnf", "Arbitrary"]
  let formulaType = "Arbitrary"
  formulaConfig <- case formulaType of
    "Cnf" -> FormulaCnf <$> validBoundsCnf
    "Dnf" -> FormulaDnf <$> validBoundsCnf
    _ -> FormulaArbitrary <$> validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
            maxNodes < 30 &&
            minAmountOfUniqueAtoms == fromIntegral (length availableAtoms)

  percentageOfGaps <- choose (1, 100)
  percentTrueEntriesLow' <- choose (0, 90)
  percentTrueEntriesHigh' <- choose (percentTrueEntriesLow', 100) `suchThat` (/= percentTrueEntriesLow')
  percentTrueEntries <- elements [Just (percentTrueEntriesLow', percentTrueEntriesHigh'), Nothing]

  pure $ FillConfig {
      formulaConfig
    , percentageOfGaps
    , percentTrueEntries
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "BaseConfig" $ do
    it "default base config should pass config check" $
      doesNotRefuse (checkBaseConf dBaseConf :: LangM Maybe)
    it "validBoundsBase should generate a valid config" $
      forAll validBoundsBase $ \baseConfig ->
        doesNotRefuse (checkBaseConf baseConfig :: LangM Maybe)
  describe "NormalFormConfig" $ do
    it "default cnf config should pass config check" $
      doesNotRefuse (checkNormalFormConfig dNormalFormConf :: LangM Maybe)
    it "validBoundsCnf should generate a valid config" $
      forAll validBoundsCnf $ \normalFormConfig ->
        doesNotRefuse (checkNormalFormConfig normalFormConfig :: LangM Maybe)
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dFillConf :: LangM Maybe)
    it "validBoundsFill should generate a valid config" $
      forAll validBoundsFill $ \fillConfig ->
        doesNotRefuse (verifyQuiz fillConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsFill $ \fillConfig@FillConfig{..} -> do
        forAll (genFillInst fillConfig) $ \inst ->
          doesNotRefuse (description False inst :: LangM Maybe)
  describe "genFillInst" $ do
    it "should generate an instance with the right amount of gaps" $
      forAll validBoundsFill $ \fillConfig@FillConfig{..} -> do
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          let tableLen = length (getEntries (getTable formula))
              gapCount = max (tableLen * percentageOfGaps `div` 100) 1 in
          length missing == gapCount
    it "generated formula should depend on all atomics" $
     forAll validBoundsFill $ \fillConfig@FillConfig{..} -> do
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          formulaDependsOnAllAtoms formula
    it "should respect percentTrueEntries" $
      forAll validBoundsFill $ \fillConfig@FillConfig{..} ->
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          withRatio (fromMaybe (0, 100) percentTrueEntries) formula
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsFill $ \fillConfig -> do
        forAll (genFillInst fillConfig) $ \fillInst ->
          doesNotRefuse (verifyStatic fillInst :: LangM Maybe)
    it "the generated solution should pass grading" $
      forAll validBoundsFill $ \fillConfig -> do
        forAll (genFillInst fillConfig) $ \fillInst ->
          doesNotRefuse (partialGrade fillInst (map TruthValue (missingValues fillInst))  :: LangM Maybe) &&
          doesNotRefuse (completeGrade fillInst (map TruthValue (missingValues fillInst))  :: Rated Maybe)

