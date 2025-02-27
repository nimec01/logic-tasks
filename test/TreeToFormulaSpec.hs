{-# LANGUAGE NamedFieldPuns #-}
module TreeToFormulaSpec where
import Test.Hspec (Spec, describe, it)
import Control.OutputCapable.Blocks (LangM)
import TestHelpers (doesNotRefuse, doesNotRefuseIO)
import Tasks.TreeToFormula.Config (checkTreeToFormulaConfig, defaultTreeToFormulaConfig, TreeToFormulaInst (tree), TreeToFormulaConfig (..))
import LogicTasks.Syntax.TreeToFormula (description, verifyInst, partialGrade', completeGrade')
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck (ioProperty, forAll, Gen)
import Tasks.TreeToFormula.Quiz (generateTreeToFormulaInst)
import Trees.Types (TreeFormulaAnswer(TreeFormulaAnswer))
import SynTreeSpec (validBoundsSynTree)



validBoundsTreeToFormula :: Gen TreeToFormulaConfig
validBoundsTreeToFormula = do
  syntaxTreeConfig <- validBoundsSynTree
  pure $ TreeToFormulaConfig
    { syntaxTreeConfig
    , extraHintsOnSemanticEquivalence = True
    , extraHintsOnAssociativity = True
    , extraText = Nothing
    , printSolution = False
    , offerUnicodeInput = False
    }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (checkTreeToFormulaConfig defaultTreeToFormulaConfig :: LangM Maybe)
    it "validBoundsTreeToFormula should generate a valid config" $
      forAll validBoundsTreeToFormula $ \config ->
        doesNotRefuse (checkTreeToFormulaConfig config :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsTreeToFormula $ \config ->
        forAll (generateTreeToFormulaInst config) $ \inst -> ioProperty $
          withSystemTempDirectory "logic-tasks" $ \path ->
            doesNotRefuseIO (description path inst)
  describe "generateTreeToFormulaInst" $ do
    it "should pass verifyInst" $
      forAll validBoundsTreeToFormula $ \config ->
        forAll (generateTreeToFormulaInst config) $ \inst ->
          doesNotRefuse
            (verifyInst inst :: LangM Maybe)
    it "possible solution passes partialGrade" $
      forAll validBoundsTreeToFormula $ \config ->
        forAll (generateTreeToFormulaInst config) $ \inst ->
          doesNotRefuse
            (partialGrade' inst $ TreeFormulaAnswer (Just $ tree inst) :: LangM Maybe)
    it "possible solution passes completeGrade" $
      forAll validBoundsTreeToFormula $ \config ->
        forAll (generateTreeToFormulaInst config) $ \inst -> ioProperty $
          withSystemTempDirectory "logic-tasks" $ \path ->
            doesNotRefuseIO (completeGrade' path inst $ TreeFormulaAnswer (Just $ tree inst))
