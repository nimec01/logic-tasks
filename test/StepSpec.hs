{-# LANGUAGE NamedFieldPuns #-}
module StepSpec where

import Test.Hspec (Spec, describe, it)
import Control.OutputCapable.Blocks (LangM)
import TestHelpers (doesNotRefuse)
import Test.QuickCheck (forAll, Gen)
import Config (StepConfig(..), StepInst(solution), dStepConf, StepAnswer (StepAnswer))
import FillSpec (validBoundsBase)
import LogicTasks.Semantics.Step (verifyQuiz, genStepInst, description, verifyStatic, partialGrade', completeGrade')


validBoundsStep :: Gen StepConfig
validBoundsStep = do
  baseConf <- validBoundsBase
  pure $ StepConfig
    { baseConf
    , useSetNotation = False
    , printSolution = False
    , extraText = Nothing
    , offerUnicodeInput = False
    }



spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dStepConf :: LangM Maybe)
    it "validBoundsStep should generate a valid config" $
      forAll validBoundsStep $ \config ->
        doesNotRefuse (verifyQuiz config :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsStep $ \config ->
        forAll (genStepInst config) $ \inst ->
          doesNotRefuse (description True inst :: LangM Maybe)
  describe "generateStepInst" $ do
    it "should pass verifyStatic" $
      forAll validBoundsStep $ \config ->
        forAll (genStepInst config) $ \inst ->
          doesNotRefuse
            (verifyStatic inst :: LangM Maybe)
    it "possible solution passes partialGrade" $
      forAll validBoundsStep $ \config ->
        forAll (genStepInst config) $ \inst ->
          doesNotRefuse (partialGrade' inst $ StepAnswer $ Just $ solution inst :: LangM Maybe)
    it "possible solution passes completeGrade" $
      forAll validBoundsStep $ \config ->
        forAll (genStepInst config) $ \inst ->
          doesNotRefuse (completeGrade' inst $ StepAnswer $ Just $ solution inst :: LangM Maybe)
