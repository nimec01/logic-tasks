{-# LANGUAGE RecordWildCards #-}
module DecomposeFormulaSpec where

import Test.Hspec
import Tasks.DecomposeFormula.Config (
  DecomposeFormulaConfig(..),
  checkDecomposeFormulaConfig,
  defaultDecomposeFormulaConfig,
  DecomposeFormulaInst(..))
import Test.QuickCheck
import SynTreeSpec (validBoundsSynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Control.OutputCapable.Blocks (LangM)
import Data.Maybe (fromJust)
import Tasks.DecomposeFormula.Quiz (generateDecomposeFormulaInst)
import Trees.Helpers (bothKids, binOp, swapKids)
import Trees.Types (SynTree(..), BinOp(..), TreeFormulaAnswer (TreeFormulaAnswer))
import Trees.Print (display)
import qualified Data.Map as Map (fromList)
import LogicTasks.Syntax.DecomposeFormula (verifyInst, description, partialGrade', completeGrade')
import TestHelpers (doesNotRefuse, doesNotRefuseIO)
import System.IO.Temp (withSystemTempDirectory)

validBoundsDecomposeFormula :: Gen DecomposeFormulaConfig
validBoundsDecomposeFormula = do
  syntaxTreeConfig <- validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
    minUniqueBinOperators >= 1 && minUniqueBinOperators < 4 && minNodes > 6
  return DecomposeFormulaConfig {
    syntaxTreeConfig = syntaxTreeConfig {
      binOpFrequencies = Map.fromList
        [ (And, 1)
        , (Or, 1)
        , (Impl, 0)
        , (BackImpl, 0)
        , (Equi, 1)
        ]
    },
    extraHintsOnAssociativity = False,
    extraText = Nothing,
    printSolution = False,
    offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (checkDecomposeFormulaConfig defaultDecomposeFormulaConfig :: LangM Maybe)
    it "validBoundsDecomposeFormula should generate a valid config" $
      forAll validBoundsDecomposeFormula $ \decomposeFormulaConfig ->
        doesNotRefuse (checkDecomposeFormulaConfig decomposeFormulaConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsDecomposeFormula $ \config -> do
        forAll (generateDecomposeFormulaInst config) $ \inst ->
          doesNotRefuse (description inst :: LangM Maybe)
  describe "generateDecomposeFormulaInst" $ do
    it "the generated instance should pass verifyInst" $
      forAll validBoundsDecomposeFormula $ \config -> do
        forAll (generateDecomposeFormulaInst config) $ \inst ->
          doesNotRefuse (verifyInst inst :: LangM Maybe)
    it "should pass partialGrade with correct answer" $
      forAll validBoundsDecomposeFormula $ \config@DecomposeFormulaConfig{..} -> do
        forAll (generateDecomposeFormulaInst config) $ \inst ->
          doesNotRefuse (partialGrade' inst (TreeFormulaAnswer $ Just $ swapKids $ tree inst) :: LangM Maybe)
    it "should pass completeGrade with correct answer" $
      forAll validBoundsDecomposeFormula $ \config@DecomposeFormulaConfig{..} -> do
        forAll (generateDecomposeFormulaInst config) $ \inst ->
          ioProperty $
            withSystemTempDirectory "logic-tasks" $ \path ->
              doesNotRefuseIO (completeGrade' path inst (TreeFormulaAnswer $ Just $ swapKids $ tree inst))
    it "should generate an instance with different subtrees" $
      forAll validBoundsDecomposeFormula $ \decomposeFormulaConfig ->
        forAll (generateDecomposeFormulaInst decomposeFormulaConfig) $ \DecomposeFormulaInst{..} ->
          let (lk,rk) = bothKids tree
              rootOp = fromJust $ binOp tree
          in notElem (display (Binary rootOp lk rk)) [display (Binary rootOp rk lk), reverse (display (Binary rootOp lk rk))]
