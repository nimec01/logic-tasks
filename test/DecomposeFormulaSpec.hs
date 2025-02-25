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
import Data.Maybe (isJust, fromJust)
import Control.Monad.Identity (Identity(runIdentity))
import Control.OutputCapable.Blocks.Generic (evalLangM)
import Tasks.DecomposeFormula.Quiz (generateDecomposeFormulaInst)
import Trees.Helpers (bothKids, binOp)
import Trees.Types (SynTree(..), BinOp(..), TreeFormulaAnswer (TreeFormulaAnswer))
import Trees.Print (display)
import qualified Data.Map as Map (fromList)
import LogicTasks.Syntax.DecomposeFormula (verifyInst, description, partialGrade')

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
      isJust $ runIdentity $ evalLangM (checkDecomposeFormulaConfig defaultDecomposeFormulaConfig :: LangM Maybe)
    it "validBoundsDecomposeFormula should generate a valid config" $
      forAll validBoundsDecomposeFormula $ \decomposeFormulaConfig ->
        isJust $ runIdentity $ evalLangM (checkDecomposeFormulaConfig decomposeFormulaConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsDecomposeFormula $ \config -> do
        forAll (generateDecomposeFormulaInst config) $ \inst ->
          isJust $ runIdentity $ evalLangM (description inst :: LangM Maybe)
  describe "generateDecomposeFormulaInst" $ do
    it "the generated instance should pass verifyInst" $
      forAll validBoundsDecomposeFormula $ \config -> do
        forAll (generateDecomposeFormulaInst config) $ \inst ->
          isJust $ runIdentity $ evalLangM (verifyInst inst :: LangM Maybe)
    it "should pass grading with correct answer" $
      forAll validBoundsDecomposeFormula $ \config@DecomposeFormulaConfig{..} -> do
        forAll (generateDecomposeFormulaInst config) $ \inst ->
          isJust (runIdentity (evalLangM (partialGrade' inst (TreeFormulaAnswer $ Just $ tree inst) :: LangM Maybe)))
          -- evalLangM does not satisfy MonadIO constraint
          -- && isJust (runIdentity (evalLangM (completeGrade' inst (TreeFormulaAnswer $ Just $ tree inst) :: LangM Maybe)))
    it "should generate an instance with different subtrees" $
      forAll validBoundsDecomposeFormula $ \decomposeFormulaConfig ->
        forAll (generateDecomposeFormulaInst decomposeFormulaConfig) $ \DecomposeFormulaInst{..} ->
          let (lk,rk) = bothKids tree
              rootOp = fromJust $ binOp tree
          in notElem (display (Binary rootOp lk rk)) [display (Binary rootOp rk lk), reverse (display (Binary rootOp lk rk))]
