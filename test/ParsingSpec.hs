module ParsingSpec where

import Parsing (formulaParse, illegalPropositionStringParse)
import Data.Either (isRight)

import Test.Hspec ( describe, it, Spec)

spec :: Spec
spec =
  describe "formulaParse" $ do
    it "correctly recognizes simple double negation" $
      isRight $ formulaParse "~~A"
    it "correctly recognizes simple set" $
      isRight $ illegalPropositionStringParse "{1,2,3,12,2}"
