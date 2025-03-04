module MinMaxSpec where

import Test.Hspec (Spec, describe, it)
import Control.OutputCapable.Blocks (LangM)
import TestHelpers (doesNotRefuse)
import Test.QuickCheck (forAll)
import qualified LogicTasks.Semantics.Max as Max (verifyQuiz, verifyStatic, genMaxInst, description, partialGrade', completeGrade')
import qualified LogicTasks.Semantics.Min as Min (verifyQuiz, verifyStatic, genMinInst, description, partialGrade', completeGrade')
import Config (dMinMaxConf, MaxInst (cnf), MinInst (dnf))



spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (Max.verifyQuiz dMinMaxConf :: LangM Maybe) &&
      doesNotRefuse (Min.verifyQuiz dMinMaxConf :: LangM Maybe)
  describe "description" $ do
    it "should not reject - Max" $
      forAll (Max.genMaxInst dMinMaxConf) $ \inst ->
        doesNotRefuse (Max.description inst :: LangM Maybe)
    it "should not reject - Min" $
      forAll (Min.genMinInst dMinMaxConf) $ \inst ->
        doesNotRefuse (Min.description inst :: LangM Maybe)
  describe "generateInst" $ do
    it "should pass verifyStatic - Max" $
      forAll (Max.genMaxInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Max.verifyStatic inst :: LangM Maybe)
    it "should pass verifyStatic - Min" $
      forAll (Min.genMinInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Min.verifyStatic inst :: LangM Maybe)
    it "possible solution passes partialGrade - Max" $
      forAll (Max.genMaxInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Max.partialGrade' inst $ cnf inst :: LangM Maybe)
    it "possible solution passes partialGrade - Min" $
      forAll (Min.genMinInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Min.partialGrade' inst $ dnf inst :: LangM Maybe)
    it "possible solution passes completeGrade - Max" $
      forAll (Max.genMaxInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Max.completeGrade' inst $ cnf inst :: LangM Maybe)
    it "possible solution passes completeGrade - Min" $
      forAll (Min.genMinInst dMinMaxConf) $ \inst ->
        doesNotRefuse
          (Min.completeGrade' inst $ dnf inst :: LangM Maybe)
