module ParsingSpec where

import Parsing (normParse)
import Data.Either (isRight)

import Test.Hspec ( describe, it, Spec )

spec :: Spec
spec =
  describe "normParse" $
    it "correctly recognizes simple double negation" $
      isRight $ normParse "~~A"
