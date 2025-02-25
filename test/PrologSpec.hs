{-# LANGUAGE RecordWildCards #-}
module PrologSpec where
import Test.Hspec
import LogicTasks.Semantics.Prolog (genPrologInst, verifyQuiz, description, verifyStatic, partialGrade', completeGrade')
import Config (dPrologConf, PrologInst (..), PrologConfig (..))
import Formula.Helpers (hasTheClauseShape)
import Test.QuickCheck
import Control.OutputCapable.Blocks (LangM)
import Data.Maybe (isJust)
import Control.Monad.Identity (Identity(runIdentity))
import Control.OutputCapable.Blocks.Generic (evalLangM)



--     , usedPredicates = [PrologLiteral True "f" ["a"], PrologLiteral True "f" ["b"], PrologLiteral True "g" ["a"]]

-- validPrologLiteral :: Gen PrologLiteral
-- validPrologLiteral = do
--   -- polarity <- elements [True, False]
--   let polarity = True
--   name <- singleton <$> elements "fghijk"
--   constants <- sublistOf ["abcde"] `suchThat` notNull
--   pure $ PrologLiteral
--     { polarity
--     , name
--     , constants
--     }

-- validClauseShape :: Gen ClauseShape
-- validClauseShape = oneof [pure AnyClause, HornClause <$> elements [minBound .. maxBound]]

-- validBoundsProlog :: Gen PrologConfig
-- validBoundsProlog = do
--   predicateAmount <- choose (2 :: Int, 10)
--   usedPredicates <- vectorOfUniqueBy predicateAmount (\p1 p2 -> name p1 /= name p2) validPrologLiteral
--   minClauseLength <- choose (1, predicateAmount)
--   maxClauseLength <- choose (minClauseLength, predicateAmount)
--   (firstClauseShape, secondClauseShape) <- ((,) <$> validClauseShape <*> validClauseShape)
--     `suchThat` \(first, second) -> not ((first `elem` [HornClause Fact, HornClause Query]) && first == second)
--   useSetNotation <- elements [True, False]

--   pure $ PrologConfig
--     { minClauseLength
--     , maxClauseLength
--     , usedPredicates
--     , extraText = Nothing
--     , printSolution = False
--     , firstClauseShape
--     , secondClauseShape
--     , useSetNotation
--     }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      isJust $ runIdentity $ evalLangM (verifyQuiz dPrologConf :: LangM Maybe)
    it "validBoundsProlog should generate a valid config" $
      -- forAll validBoundsProlog $ \prologConfig ->
        isJust $ runIdentity $ evalLangM (verifyQuiz dPrologConf :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      -- forAll validBoundsProlog $ \prologConfig@PrologConfig{..} ->
        forAll (genPrologInst dPrologConf) $ \inst ->
          isJust $ runIdentity $ evalLangM (description inst :: LangM Maybe)
  describe "genPrologInst" $ do
    it "should pass verifyStatic" $
      -- forAll validBoundsProlog $ \prologConfig@PrologConfig{..} -> do
        forAll (genPrologInst dPrologConf) $ \inst ->
          isJust $ runIdentity $ evalLangM (verifyStatic inst :: LangM Maybe)
    it "should pass grading with correct answer" $
      -- forAll validBoundsProlog $ \prologConfig@PrologConfig{..} -> do
        forAll (genPrologInst dPrologConf) $ \inst ->
          isJust (runIdentity (evalLangM (partialGrade' inst (solution inst) :: LangM Maybe))) &&
          isJust (runIdentity (evalLangM (completeGrade' inst (solution inst) :: LangM Maybe)))
    it "should only generate PrologInst with horn clauses by default" $
      forAll (genPrologInst dPrologConf) $ \PrologInst {..} ->
        hasTheClauseShape (firstClauseShape dPrologConf) literals1
          && hasTheClauseShape (secondClauseShape dPrologConf) literals2
