{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module ComposeFormulaSpec where

import Test.Hspec
import Tasks.ComposeFormula.Config (
  ComposeFormulaConfig(..),
  TreeDisplayMode (FormulaDisplay),
  checkComposeFormulaConfig,
  defaultComposeFormulaConfig,
  ComposeFormulaInst(..))
import Test.QuickCheck
import SynTreeSpec (validBoundsSynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Control.OutputCapable.Blocks (LangM)
import Data.Maybe (isJust)
import Tasks.ComposeFormula.Quiz (generateComposeFormulaInst)
import Trees.Types (SynTree(Binary), TreeFormulaAnswer (TreeFormulaAnswer))
import LogicTasks.Syntax.ComposeFormula (partialGrade', description, completeGrade', verifyInst)
import TestHelpers (doesNotRefuse, doesNotRefuseIO)
import System.IO.Temp (withSystemTempDirectory)

validBoundsComposeFormula :: Gen ComposeFormulaConfig
validBoundsComposeFormula = do
  syntaxTreeConfig <- validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
    minUniqueBinOperators >= 1 && minNodes > 6
  displayModeL <- elements [minBound..maxBound :: TreeDisplayMode]
  displayModeR <- elements [minBound..maxBound :: TreeDisplayMode]
  return ComposeFormulaConfig {
    syntaxTreeConfig,
    treeDisplayModes = (displayModeL, displayModeR),
    extraHintsOnAssociativity = False,
    extraText = Nothing,
    printSolution = False,
    offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (checkComposeFormulaConfig defaultComposeFormulaConfig :: LangM Maybe)
    it "validBoundsComposeFormula should generate a valid config" $
      forAll validBoundsComposeFormula $ \composeFormulaConfig ->
        doesNotRefuse (checkComposeFormulaConfig composeFormulaConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsComposeFormula $ \config ->
        forAll (generateComposeFormulaInst config) $ \inst -> ioProperty $
          withSystemTempDirectory "logic-tasks" $ \path ->
            doesNotRefuseIO (description False path inst)
  describe "generateComposeFormulaInst" $ do
    it "should pass verifyInst" $
      forAll validBoundsComposeFormula $ \composeFormulaConfig ->
        forAll (generateComposeFormulaInst composeFormulaConfig) $ \inst ->
          doesNotRefuse
            (verifyInst inst :: LangM Maybe)
    it "possible solution passes partialGrade" $
      forAll validBoundsComposeFormula $ \composeFormulaConfig ->
        forAll (generateComposeFormulaInst composeFormulaConfig) $ \inst@ComposeFormulaInst{..} ->
          let lrTree = Binary operator leftTree rightTree
              rlTree = Binary operator rightTree leftTree
          in doesNotRefuse
            (partialGrade' inst [TreeFormulaAnswer (Just lrTree), TreeFormulaAnswer (Just rlTree)] :: LangM Maybe)
    it "possible solution passes completeGrade" $
      forAll validBoundsComposeFormula $ \composeFormulaConfig ->
        forAll (generateComposeFormulaInst composeFormulaConfig) $ \inst@ComposeFormulaInst{..} ->
          let lrTree = Binary operator leftTree rightTree
              rlTree = Binary operator rightTree leftTree
          in ioProperty $
            withSystemTempDirectory "logic-tasks" $ \path ->
              doesNotRefuseIO (completeGrade' path inst [TreeFormulaAnswer (Just lrTree), TreeFormulaAnswer (Just rlTree)])
    it "leftTreeImage and rightTreeImage has the right value" $
      forAll validBoundsComposeFormula $ \composeFormulaConfig@ComposeFormulaConfig{..} ->
        forAll (generateComposeFormulaInst composeFormulaConfig) $ \ComposeFormulaInst{..} ->
          fst treeDisplayModes == FormulaDisplay || isJust leftTreeImage &&
          snd treeDisplayModes == FormulaDisplay || isJust rightTreeImage
