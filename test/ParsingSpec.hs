module ParsingSpec where

import Parsing (formulaParse)
import Data.Either (isRight)

import Test.Hspec ( describe, it, Spec)

spec :: Spec
spec =
  describe "formulaParse" $
    it "correctly recognizes simple double negation" $
      isRight $ formulaParse "~~A"
