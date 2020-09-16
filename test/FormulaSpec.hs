module FormulaSpec where



import Test.Hspec
import Test.QuickCheck
import Formula
import Control.Exception(evaluate)
import qualified Data.Set as Set
import Data.Set (empty,Set)
import Data.List (nub)



allocGen :: Gen Allocation
allocGen = do
    alloc <- arbitrary
    let flipped = flipNegs alloc
    pure flipped
  where
    flipNegs = map flipper
    flipper x
        | negative first = (opposite first,not (snd x))
        | otherwise      = x
      where
        first = fst x
    negative (Not _) = True
    negative _ = False



validBoundsClause :: Gen ((Int,Int),[Char])
validBoundsClause = do
    validChars <- sublistOf ['A'..'Z']
    let upperBound = length validChars
    lower <- chooseInt (1,upperBound)
    upper <- chooseInt (lower,upperBound)
    pure ((lower,upper),validChars)



validBoundsCnf :: Gen ((Int,Int),(Int,Int),[Char])
validBoundsCnf = do
    ((minLen,maxLen),chars) <- validBoundsClause
    let upperBound = minimum [2^ maxLen, 2^length chars]
    minNum <- chooseInt (1,upperBound)
    maxNum <- chooseInt (minNum,upperBound)
    pure ((minNum,maxNum),(minLen,maxLen),chars)



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
        let len = Set.size (getLs clause) in len >= lower && len <= upper

  describe "evalClause" $ do
    it "should return Nothing when called with an empty allocation" $
      property $ \clause -> clause /= Clause empty ==> evalClause [] clause `shouldBe` Nothing
    it "should return Nothing when called with a mismatched allocation" $
      property $ \clause -> forAll allocGen $
        \alloc -> not (all (`elem` map fst alloc) (map turnPositive (Set.toList (getLs clause))))
          ==> evalClause alloc clause `shouldBe` Nothing


  describe "genCnf" $ do
    it "should return the empty conjuncion when called with the empty list" $
      property $ \bounds1 bounds2 -> forAll (genCnf bounds1 bounds2 []) (== Cnf empty)
    it "should return the empty conjunction when called with invalid boundaries" $
      property $ \lower1 upper1 lower2 upper2 lits -> lower1 <= 0 || lower2 <= 0
                 || upper1 < lower1 || upper2 < lower2 || lower1 > minimum [2^lower2, 2^length (nub lits)]
                   ==> forAll (genCnf (lower1,upper1) (lower2,upper2) lits) (== Cnf empty)
    it "should generate a random cnf formula with a correct amount of clauses if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) -> forAll (genCnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \cnf ->
        let num = Set.size (getCs cnf) in num >= lowerNum && num <= upperNum
    it "should generate a random cnf formula with the correct clause length if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) -> forAll (genCnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \cnf ->
       let sizes = Set.map Set.size (Set.map getLs (getCs cnf)) in Set.findMax sizes <= upperLen && Set.findMin sizes >= lowerLen
