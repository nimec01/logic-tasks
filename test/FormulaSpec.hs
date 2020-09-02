module FormulaSpec where

import Test.Hspec
import Test.QuickCheck
import Formula
import Control.Exception(evaluate)



allocGen :: Gen Allocation
allocGen = do
 alloc <- arbitrary
 let flipped = flipNegs alloc 
 return flipped
  where flipNegs = map (\x -> let first = fst x in if negative first 
                                                     then (opposite first,not (snd x)) 
                                                     else x)
        negative (Not _) = True
        negative _ = False
              



spec :: Spec
spec = do
  describe "genLiteral" $ do
    it "should throw an error when called with the empty list" $
      evaluate (genLiteral []) `shouldThrow` errorCall "Can not construct Literal from empty list."
    it "should generate a random literal from the given char list" $
      property $ \chars -> not (null chars) ==> forAll (genLiteral chars) $ \char -> getC char `elem` chars

  describe "evalLiteral" $ do
    it "should return Nothing when called with an empty allocation" $
      property $ \lit -> evalLiteral [] lit == Nothing
    it "should return Nothing when called with a mismatched allocation" $
      property $ \lit -> forAll allocGen $ \alloc -> 
                           turnPositive lit `notElem` map fst alloc
                             ==> evalLiteral alloc lit == Nothing 
    it "should produce the correct boolean value when called with matching allocation and literal" $
      property $ \lit -> forAll (allocGen) $ \alloc -> let allocLits = map fst alloc in 
                                  lit `elem` allocLits || opposite lit `elem` allocLits 
                                    ==> evalLiteral alloc lit == case lit of 
                                      Literal x -> lookup lit alloc 
                                      _         -> not <$> lookup (opposite lit) alloc                                 