module TypesSpec where

import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "checkFillConfig" $ do
    context "when used with defaultFillConfig" $ do 
      it "returns Nothing" $ do
        checkFillConfig defaultFillConfig `shouldBe` Nothing

  describe "checkGiveCnfConfig" $ do
    context "when used with defaultGiveCnfConfig" $ do 
      it "returns Nothing" $ do
        checkGiveCnfConfig defaultGiveCnfConfig `shouldBe` Nothing

  describe "checkPickConfig" $ do
    context "when used with defaultPickConfig" $ do 
      it "returns Nothing" $ do
        checkPickConfig defaultPickConfig `shouldBe` Nothing

  describe "checkDecideConfig" $ do
    context "when used with defaultDecideConfig" $ do 
      it "returns Nothing" $ do
        checkDecideConfig defaultDecideConfig `shouldBe` Nothing

  describe "checkStepConfig" $ do
    context "when used with defaultStepConfig" $ do 
      it "returns Nothing" $ do
        checkStepConfig defaultStepConfig `shouldBe` Nothing

  describe "checkResolutionConfig" $ do
    context "when used with defaultResolutionConfig" $ do 
      it "returns Nothing" $ do
        checkResolutionConfig defaultResolutionConfig `shouldBe` Nothing