module TableSpec where

import Data.Set (Set,empty)
import Test.Hspec
import Test.QuickCheck
import Table
import Formula
import qualified Data.Set as Set



equivGen :: Gen (CNF,CNF)
equivGen = sized equiv
  where equiv n = do
          cnf <- resize n arbitrary
          let clauses = getCs cnf
          if Set.null clauses
            then equivGen
            else do
              let literals = Set.unions (Set.map getLs clauses)
              clause <- elements (Set.toList clauses)
              let clauseLits = getLs clause
              let availLits = literals Set.\\ clauseLits
              if Set.null availLits
                then equivGen
                else do
                  chosenLit <- elements (Set.toList availLits)
                  let newClause = Clause (Set.insert chosenLit clauseLits)
                  let newCnf = CNF (Set.insert newClause clauses)
                  return (cnf,newCnf)





spec :: Spec
spec = do
  describe "getTable" $ do
    context "When generating different tables" $
      it "should have used different formulae" $
        forAll (applySize arbitrary) $ \(x,y) -> getTable x /= getTable y ==> x /= y

    context "When using equivalent formulae" $
      it "should produce the same table twice" $
        forAll (applySize equivGen) $ \(x,y) -> getTable x == getTable y

    context "When looking at each row of the generated table" $
      it "should have truth values equal to the formula being evaluated with the row's allocation" $
        forAll (applySize arbitrary) $ \x -> map (`evalCNF` x) (possibleAllocations (getLiterals x)) == readEntries (getTable x)


  describe "genGapTable" $ do
    it "should return the table argument when the amount of gaps is zero" $
       forAll (applySize arbitrary) $ \table -> forAll (genGapTable table 0) $
        \gapTable -> table `shouldBe` gapTable

    it "should return an empty table if the table parameter is empty" $
      forAll arbitrarySizedNatural $ \gaps -> let emptyTable = getTable (CNF empty) in
        forAll (genGapTable emptyTable gaps) $ \gapTable -> gapTable `shouldBe` emptyTable

  where size = 10
        applySize g = resize size g
