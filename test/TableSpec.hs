module TableSpec where

import Data.Set (toList,(\\),insert,delete)
import Test.Hspec
import Test.QuickCheck
import Table
import Formula
import qualified Data.Set as Set (null,filter)



equivGen :: Gen (CNF,CNF)
equivGen = do cnf1 <- arbitrary
              let clauses = getCs cnf1
              clause1 <- elements (toList clauses)
              let restSet = delete clause1 clauses
              let compatibleSets = Set.filter (\set -> not (Set.null (getLs set \\ getLs clause1))) restSet
              if Set.null compatibleSets
                then return (cnf1,cnf1)
                else do clause2 <- elements (toList compatibleSets)
                        let possibleLits = getLs clause2 \\ getLs clause1
                        lit <- elements (toList possibleLits)
                        let newSet = insert (Clause (insert lit (getLs clause1))) (getCs cnf1)
                        let cnf2 = CNF newSet
                        return (cnf1,cnf2)



spec :: Spec
spec =
  describe "getTable" $ do
    context "When generating different tables" $
      it "should have used different formulae" $
        property $ \x y -> getTable x /= getTable y ==> x /= y

    context "When using equivalent formulae" $
      it "should produce the same table twice" $
        forAll equivGen $ \(x,y) -> getTable x == getTable y

    context "When looking at each row of the generated table" $
      it "should have truth values equal to the formula being evaluated with the row's allocation" $
        property $ \x -> map (`evalCNF` x) (possibleAllocations (getLiterals x)) == readEntries (getTable x)
