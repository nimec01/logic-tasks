{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalPropositionSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.List ((\\))
import Data.Char (isLetter)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, forAll, suchThat, within, ioProperty)

import Tasks.LegalProposition.Config (
  LegalPropositionConfig (..),
  LegalPropositionInst(..),
  checkLegalPropositionConfig,
  defaultLegalPropositionConfig)
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.LegalProposition.PrintBracket (bracketDisplay,)
import Tasks.LegalProposition.Quiz (generateLegalPropositionInst)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Parsing (formulaParse)
import Trees.Generate (genSynTree)
import SynTreeSpec (validBoundsSynTree)
import Trees.Print (display)
import TestHelpers (deleteBrackets, deleteSpaces, doesNotRefuse, doesNotRefuseIO)
import Control.OutputCapable.Blocks (LangM)
import Data.Maybe (isNothing)
import Tasks.LegalProposition.Helpers (formulaAmount)
import LogicTasks.Syntax.IllegalFormulas (description, partialGrade, verifyInst, completeGrade)
import System.IO.Temp (withSystemTempDirectory)

validBoundsLegalProposition :: Gen LegalPropositionConfig
validBoundsLegalProposition = do
    formulas <- choose (1, 15)
    syntaxTreeConfig@SynTreeConfig {..}  <- validBoundsSynTree
      `suchThat` \cfg -> formulaAmount cfg >= formulas
    illegals <- choose (0, formulas)
    bracketFormulas <- choose (0, formulas - illegals)
    return $ LegalPropositionConfig
        {
            syntaxTreeConfig
            , formulas
            , illegals
            , bracketFormulas
            , extraText = Nothing
            , printSolution = False
        }

timeout :: Int
timeout = 30000000 -- 30 seconds

spec :: Spec
spec = do
    describe "config" $ do
      it "default config should pass config check" $
        doesNotRefuse (checkLegalPropositionConfig defaultLegalPropositionConfig :: LangM Maybe)
      it "validBoundsLegalProposition should generate a valid config" $
        forAll validBoundsLegalProposition $ \legalPropConfig ->
          doesNotRefuse (checkLegalPropositionConfig legalPropConfig :: LangM Maybe)
    describe "description" $ do
      it "should not reject" $
        within timeout $ forAll validBoundsLegalProposition $ \config ->
          forAll (generateLegalPropositionInst config) $ \inst ->
            doesNotRefuse (description False inst :: LangM Maybe)
    describe "illegalDisplay" $ do
        it "at least creates actual formula symbols" $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (deleteSpaces <$> illegalDisplay synTree) $
                      all (\c -> c `elem` "()∧∨¬<=>" || isLetter c)
        it "the string after illegalDisplay cannot be parsed" $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (illegalDisplay synTree) $ \str -> isLeft (formulaParse str)
    describe "bracket display" $ do
        it "the String after bracketDisplay just add a bracket " $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> length str == length (display synTree) + 2
        it "the String can be parsed by formulaParse" $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> formulaParse str == Right synTree
        it "the String remove all brackets should same with display remove all brackets" $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> deleteBrackets str == deleteBrackets (display synTree)
    describe "generateLegalPropositionInst" $ do
        it "the generateLegalPropositionInst should generate expected illegal number" $
            within timeout $ forAll validBoundsLegalProposition $ \config ->
                forAll (generateLegalPropositionInst config) $ \LegalPropositionInst{..} ->
                  let serialsOfWrong = map fst $ filter (\(_,(_,mt)) -> isNothing mt) (zip [1..] pseudoFormulas) in
                    all (\x -> isLeft (formulaParse (fst (pseudoFormulas !! (x - 1))))) serialsOfWrong
        it "the generateLegalPropositionInst should generate expected legal number" $
            within timeout $ forAll validBoundsLegalProposition $ \config@LegalPropositionConfig{..} ->
                forAll (generateLegalPropositionInst config) $ \LegalPropositionInst{..} ->
                  let serialsOfWrong = map fst $ filter (\(_,(_,mt)) -> isNothing mt) (zip [1..] pseudoFormulas) in
                    all
                    (\x -> isRight (formulaParse (fst (pseudoFormulas !! (x - 1)))))
                    ([1 .. fromIntegral formulas] \\ serialsOfWrong)
        it "the generateLegalPropositionInst should pass verifyStatic" $
            within timeout $ forAll validBoundsLegalProposition $ \config@LegalPropositionConfig{..} ->
                forAll (generateLegalPropositionInst config) $ \inst ->
                   doesNotRefuse (verifyInst inst :: LangM Maybe)
        it "the generateLegalPropositionInst should pass grading" $
            within timeout $ forAll validBoundsLegalProposition $ \config@LegalPropositionConfig{..} ->
                forAll (generateLegalPropositionInst config) $ \inst ->
                   doesNotRefuse (partialGrade inst [index | (index,(_, Just _)) <- zip [1..] $ pseudoFormulas inst] :: LangM Maybe)
        it "the generateLegalPropositionInst should pass grading" $
          within timeout $ forAll validBoundsLegalProposition $ \config@LegalPropositionConfig{..} ->
            forAll (generateLegalPropositionInst config) $ \inst -> ioProperty $
              withSystemTempDirectory "logic-tasks" $ \path ->
                doesNotRefuseIO (completeGrade path inst [index | (index,(_, Just _)) <- zip [1..] $ pseudoFormulas inst])
