module TypesSpec where

import Test.Hspec
import Types

spec :: Spec
spec = do
  describe "checkFillConfig" $
    context "when used with defaultFillConfig" $
      it "returns Nothing" $
        checkFillConfig defaultFillConfig `shouldBe` Nothing

  describe "checkGiveCnfConfig" $
    context "when used with defaultGiveCnfConfig" $
      it "returns Nothing" $
        checkGiveCnfConfig defaultGiveCnfConfig `shouldBe` Nothing

  describe "checkPickConfig" $
    context "when used with defaultPickConfig" $
      it "returns Nothing" $
        checkPickConfig defaultPickConfig `shouldBe` Nothing

  describe "checkDecideConfig" $
    context "when used with defaultDecideConfig" $
      it "returns Nothing" $
        checkDecideConfig defaultDecideConfig `shouldBe` Nothing

  describe "checkStepConfig" $
    context "when used with defaultStepConfig" $
      it "returns Nothing" $
        checkStepConfig defaultStepConfig `shouldBe` Nothing

  describe "checkResolutionConfig" $
    context "when used with defaultResolutionConfig" $
      it "returns Nothing" $
        checkResolutionConfig defaultResolutionConfig `shouldBe` Nothing
