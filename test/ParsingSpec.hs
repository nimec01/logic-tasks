module ParsingSpec (spec) where

import Data.Either (isLeft, isRight)
import Test.Hspec ( describe, it, Spec)

import Trees.Parsing (formulaParse)
import Tasks.SuperfluousBrackets.Parsing (superfluousBracketsExcParser)

spec :: Spec
spec = do
  describe "formulaParse" $ do
    it "correctly recognizes simple double negation" $
      isRight $ formulaParse "~~A"
    it "correctly rejects stuff that isn't strictly well-bracketed" $
      isLeft $ formulaParse "A/\\B/\\C"
    it "correctly rejects stuff with strange spaces" $
      isLeft $ formulaParse "A/ \\B"
  describe "superfluousBracketsExcParser" $ do
    it "correctly recognizes stuff that isn't strictly well-bracketed" $
      isRight $ superfluousBracketsExcParser "A/\\B/\\C"
    it "correctly rejects stuff with strange spaces" $
      isLeft $ superfluousBracketsExcParser "A/ \\B"
