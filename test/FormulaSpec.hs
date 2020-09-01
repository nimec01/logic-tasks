module FormulaSpec where

import Test.Hspec
import Test.QuickCheck
import Formula
import Control.Exception(evaluate)


spec :: Spec
spec = do
  describe "genLiteral" $ do
    it "should throw an error when called with the empty list" $ do
      evaluate (genLiteral []) `shouldThrow` errorCall "Can not construct Literal from empty list."
    it "should generate a random literal from the given char list" $ do
      property $ \x -> not (null x) ==> forAll (genLiteral x) $ \y -> getC y `elem` x