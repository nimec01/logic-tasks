module TableSpec where

import Data.Set (Set,empty)
import Data.Maybe (isNothing,fromJust)
import qualified Control.Exception as Exc(evaluate)
import Test.Hspec
import Test.QuickCheck
import Types hiding (getLiterals)
import Formula
import Table
import qualified Data.Set as Set



equivGen :: Gen (Cnf,Cnf)
equivGen = sized equiv
  where
    equiv n = do
        cnf <- resize n arbitrary
        let clauses = getCs cnf
        if Set.null clauses
          then equivGen
          else do
            let literals = Set.unions (Set.map getLs clauses)
            clause <- elements (Set.toList clauses)
            let clauseLits = getLs clause
                availLits = literals Set.\\ clauseLits
            if Set.null availLits
              then equivGen
              else do
                chosenLit <- elements (Set.toList availLits)
                let newClause = Clause (Set.insert chosenLit clauseLits)
                let newCnf = Cnf (Set.insert newClause clauses)
                pure (cnf,newCnf)



spec :: Spec
spec = do
    tableGenSpec
    utilSpec



utilSpec :: Spec
utilSpec =
  describe "fillGaps" $ do
    it "should return the table parameter if the list is empty" $
      forAll (applySize arbitrary) $ \table -> fillGaps [] table `shouldBe` table

    it "should return the empty table if the table parameter is empty" $
      property $ \bools -> let eTable = getTable (Cnf empty) in
        fillGaps bools eTable `shouldBe` eTable

    it "should return the table argument if the bool list is too large" $
      property $ \bools -> forAll (applySize arbitrary) $
        \table -> length bools > length (filter isNothing (readEntries table))
          ==> fillGaps bools table `shouldBe` table


    it "should produce a fully filled table if called with a fitting bool list" $
      forAll (applySize arbitrary) $ \table ->
        forAll (chooseInt (1,100)) $ \gaps ->
          forAll (genGapTable table gaps) $ \gapTable ->
            let gapEntries = readEntries gapTable in
              forAll (vectorOf (length (filter isNothing gapEntries)) arbitrary) $
                \bools -> bools `shouldBe`
                  [fromJust x | (x,y) <- zip (readEntries (fillGaps bools gapTable))
                                gapEntries, isNothing y]


tableGenSpec :: Spec
tableGenSpec = do
  describe "getTable" $ do
    context "When generating different tables" $
      it "should have used different formulae" $
        forAll (applySize arbitrary) $ \(x,y) -> getTable x /= getTable y ==> x /= y

    context "When using equivalent formulae" $
      it "should produce the same table twice" $
        forAll (applySize equivGen) $ \(x,y) -> getTable x == getTable y

    context "When looking at each row of the generated table" $
      it "should have truth values equal to the formula being evaluated with the row's allocation" $
        forAll (applySize arbitrary) $ \x -> map (`evaluate` x) (possibleAllocations (getLiterals x)) == readEntries (getTable x)


  describe "genGapTable" $ do
    it "should throw an error if the amount of gaps is negative" $
       forAll (applySize arbitrary) $ \table ->
         forAll (chooseInt (1,maxBound)) $
           \gaps -> Exc.evaluate (genGapTable table (-gaps)) `shouldThrow` errorCall "The amount of gaps is negative."

    it "should throw an error if the gap parameter is bigger than the size of the table" $
       forAll (applySize arbitrary) $ \table ->
         forAll (chooseInt (101,maxBound)) $
           \gaps -> Exc.evaluate (genGapTable table gaps) `shouldThrow` errorCall "gap percentage must be less than 100%."

    it "should return an empty table if the table parameter is empty" $
      forAll arbitrarySizedNatural $ \gaps -> let emptyTable = getTable (Cnf empty) in
        forAll (genGapTable emptyTable gaps) $ \gapTable -> gapTable `shouldBe` emptyTable


    it "should return a table with the correct amount of gaps when called with valid parameters" $
      forAll arbitrarySizedNatural $ \gaps -> forAll (applySize arbitrary) $
        \table -> forAll (genGapTable table gaps) $
          \gapTable -> let tableLen = length (readEntries table) in
            gaps < tableLen ==> length (filter isNothing (readEntries gapTable)) == maximum [1, gaps * tableLen `div` 100]


  describe "genWrongTable" $ do
    it "should throw an error if the amount of changes is negative" $
       forAll (applySize arbitrary) $ \table ->
         forAll (chooseInt (1,maxBound)) $
           \changes -> Exc.evaluate (genWrongTable table (-changes)) `shouldThrow` errorCall "The amount of changes is negative."

    it "should return the table argument when the amount of changes is zero" $
       forAll (applySize arbitrary) $ \table -> forAll (genWrongTable table 0) $
        \wTable -> table `shouldBe` snd wTable

    it "should return an empty table if the table parameter is empty" $
      forAll arbitrarySizedNatural $ \changes -> let emptyTable = getTable (Cnf empty) in
        forAll (genWrongTable emptyTable changes) $ \wTable -> snd wTable `shouldBe` emptyTable

    it "should return an inverted table if the gap parameter is bigger than the size of the table" $
       forAll (applySize arbitrary) $ \table -> let tabLen = length (readEntries table) in
         forAll (suchThat arbitrary (>= tabLen)) $ \changes -> forAll (genWrongTable table changes) $
          \wTable -> readEntries (snd wTable) `shouldBe` map (fmap not) (readEntries table)

    it "should return a table with the correct amount of changes when called with valid parameters" $
      forAll arbitrarySizedNatural $ \changes -> forAll (applySize arbitrary) $
        \table -> forAll (genWrongTable table changes) $
          \wTable -> let origEntries = readEntries table in
            changes < length origEntries ==>
              length [x | (x,y) <- zip origEntries (readEntries (snd wTable)), x/=y]  `shouldBe` changes



applySize :: Gen a -> Gen a
applySize = resize 8
