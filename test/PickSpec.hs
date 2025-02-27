{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module PickSpec where
import Control.OutputCapable.Blocks (LangM)
import Test.Hspec (Spec, describe, it)
import Config (dPickConf, PickConfig (..), PickInst (..), FormulaConfig(..), Number (Number))
import LogicTasks.Semantics.Pick (verifyQuiz, genPickInst, verifyStatic, description, partialGrade, completeGrade)
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen, choose, forAll, suchThat, elements)
import SynTreeSpec (validBoundsSynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Formula.Util (isSemanticEqual)
import Data.List.Extra (nubOrd, nubSort, nubBy)
import Util (withRatio)
import Formula.Types(atomics)
import FillSpec (validBoundsCnf)
import LogicTasks.Util (formulaDependsOnAllAtoms)
import TestHelpers (doesNotRefuse)

validBoundsPick :: Gen PickConfig
validBoundsPick = do
  amountOfOptions <- choose (2, 5)
  -- formulaType <- elements ["Cnf", "Dnf", "Arbitrary"]
  let formulaType = "Arbitrary"
  formulaConfig <- case formulaType of
    "Cnf" -> FormulaCnf <$> validBoundsCnf
    "Dnf" -> FormulaDnf <$> validBoundsCnf
    _ -> FormulaArbitrary <$> validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
            amountOfOptions <= 4*2^ length availableAtoms &&
            minAmountOfUniqueAtoms >= 2 &&
            minAmountOfUniqueAtoms == fromIntegral (length availableAtoms) &&
            maxNodes <= 40

  percentTrueEntries' <- (do
    percentTrueEntriesLow' <- choose (1, 90)
    percentTrueEntriesHigh' <- choose (percentTrueEntriesLow', 99) `suchThat` (/= percentTrueEntriesLow')
    return (percentTrueEntriesLow', percentTrueEntriesHigh')
    ) `suchThat` \(a,b) -> b - a >= 30

  percentTrueEntries <- elements [Just percentTrueEntries', Nothing]

  pure $ PickConfig {
      formulaConfig
    , amountOfOptions
    , percentTrueEntries
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dPickConf :: LangM Maybe)
    it "validBoundsPick should generate a valid config" $
      forAll validBoundsPick $ \pickConfig ->
        doesNotRefuse (verifyQuiz pickConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsPick $ \pickConfig@PickConfig{..} ->
        forAll (genPickInst pickConfig) $ \inst ->
          doesNotRefuse (description False inst :: LangM Maybe)
  describe "genPickInst" $ do
    it "generated formulas should not be semantically equivalent" $
      forAll validBoundsPick $ \pickConfig@PickConfig{..} ->
        forAll (genPickInst pickConfig) $ \PickInst{..} ->
          length (nubBy isSemanticEqual formulas) == amountOfOptions
    it "generated formulas should only consist of the same atomics" $
      forAll validBoundsPick $ \pickConfig ->
        forAll (genPickInst pickConfig) $ \PickInst{..} ->
          length (nubOrd (map (nubSort . atomics) formulas)) == 1
    it "generated formulas should depend on all atomics" $
      forAll validBoundsPick $ \pickConfig ->
        forAll (genPickInst pickConfig) $ \PickInst{..} ->
          all formulaDependsOnAllAtoms formulas
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsPick $ \pickConfig -> do
        forAll (genPickInst pickConfig) $ \pickInst ->
          doesNotRefuse (verifyStatic pickInst :: LangM Maybe)
    it "should respect percentTrueEntries" $
      forAll validBoundsPick $ \pickConfig@PickConfig{..} ->
        forAll (genPickInst pickConfig) $ \PickInst{..} ->
          all (withRatio (fromMaybe (0, 100) percentTrueEntries)) formulas
    it "the generated solution should pass grading" $
      forAll validBoundsPick $ \pickConfig@PickConfig{..} ->
        forAll (genPickInst pickConfig) $ \inst ->
          doesNotRefuse (partialGrade inst (Number $ Just $ correct inst)  :: LangM Maybe) &&
          doesNotRefuse (completeGrade inst (Number $ Just $ correct inst)  :: LangM Maybe)

