module ParsingSpec where

import Trees.Parsing (formulaParse)
import Tasks.LegalProposition.Parsing (illegalPropositionStringParse)
import Data.Either (isLeft, isRight)
import Tasks.SuperfluousBrackets.Parsing (superfluousBracketsExcParser)

import Test.Hspec ( describe, it, Spec)

spec :: Spec
spec = do
  describe "formulaParse" $ do
    it "correctly recognizes simple double negation" $
      isRight $ formulaParse "~~A"
    it "correctly rejects stuff that isn't strictly well-bracketed" $
      isLeft $ formulaParse "A/\\B/\\C"
    it "correctly rejects stuff with strange spaces" $
      isLeft $ formulaParse "A/ \\B"
  describe "illegalPropositionStringParse" $
    it "correctly recognizes simple set" $
      isRight $ illegalPropositionStringParse "{1,2,3,12,2}"
  describe "superfluousBracketsExcParser" $ do
    it "correctly recognizes stuff that isn't strictly well-bracketed" $
      isRight $ superfluousBracketsExcParser "A/\\B/\\C"
    it "correctly rejects stuff with strange spaces" $
      isLeft $ superfluousBracketsExcParser "A/ \\B"
