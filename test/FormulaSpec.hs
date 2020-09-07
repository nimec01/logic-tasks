module FormulaSpec where

import Test.Hspec
import Test.QuickCheck
import Formula
import Control.Exception(evaluate)
import Data.Set (toList,fromList,empty,member,size,findMax,findMin)
import qualified Data.Set as Set (map)



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


validBoundsClause :: Gen ((Int,Int),[Char])
validBoundsClause = do
 validChars <- sublistOf ['A'..'Z']
 lower <- chooseInt (1,length validChars)
 upper <- chooseInt (lower,maxBound)
 return ((lower,upper),validChars)

validBoundsCnf :: Gen ((Int,Int),(Int,Int),[Char])
validBoundsCnf = do
 ((minLen,maxLen),chars) <- validBoundsClause
 minNum <- chooseInt (1,minLen^2)
 maxNum <- chooseInt (minNum,maxBound)
 return ((minNum,maxNum),(minLen,maxLen),chars)



spec :: Spec
spec = do
  describe "genLiteral" $ do
    it "should throw an error when called with the empty list" $
      evaluate (genLiteral []) `shouldThrow` errorCall "Can not construct Literal from empty list."
    it "should generate a random literal from the given char list" $
      property $ \chars -> not (null chars) ==> forAll (genLiteral chars) $ \char -> getC char `elem` chars

  describe "evalLiteral" $ do
    it "should return Nothing when called with an empty allocation" $
      property $ \lit -> evalLiteral [] lit `shouldBe` Nothing
    it "should return Nothing when called with a mismatched allocation" $
      property $ \lit -> forAll allocGen $ \alloc ->
                           turnPositive lit `notElem` map fst alloc
                             ==> evalLiteral alloc lit `shouldBe` Nothing



  describe "genClause" $ do
    it "should return the empty Clause when called with the empty list" $
      property $ \bounds -> forAll (genClause bounds []) (== Clause empty)
    it "should return the empty Clause when called with invalid boundaries" $
      property $ \lower upper lits -> lower <= 0 || upper < lower || lower > length lits
                   ==> forAll (genClause (lower,upper) lits) (== Clause empty)
    it "should generate a random clause of the correct length if given valid parameters" $
      forAll validBoundsClause $ \((lower,upper),chars) -> forAll (genClause (lower,upper) chars) $ \clause ->
        let len = size (getLs clause) in len >= lower && len <= upper

  describe "evalClause" $ do
    it "should return Nothing when called with an empty allocation" $
      property $ \clause -> clause /= Clause empty ==> evalClause [] clause `shouldBe` Nothing
    it "should return Nothing when called with a mismatched allocation" $
      property $ \clause -> forAll allocGen $
        \alloc -> not (all (`elem` map fst alloc) (map turnPositive (toList (getLs clause))))
          ==> evalClause alloc clause `shouldBe` Nothing


  describe "genCNF" $ do
    it "should return the empty conjuncion when called with the empty list" $
      property $ \bounds1 bounds2 -> forAll (genCNF bounds1 bounds2 []) (== CNF empty)
    it "should return the empty conjunction when called with invalid boundaries" $
      property $ \lower1 upper1 lower2 upper2 lits -> lower1 <= 0 || lower2 <= 0
                 || upper1 < lower1 || upper2 < lower2 || lower1 > lower2^2
                   ==> forAll (genCNF (lower1,upper1) (lower2,upper2) lits) (== CNF empty)
    it "should generate a random cnf formula with a correct amount of clauses if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) -> forAll (genCNF (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \cnf ->
        let num = size (getCs cnf) in num >= lowerNum && num <= upperNum
    it "should generate a random cnf formula with the correct clause length if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) -> forAll (genCNF (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \cnf ->
       let sizes = Set.map size (Set.map getLs (getCs cnf)) in findMax sizes <= upperLen && findMin sizes >= lowerLen
