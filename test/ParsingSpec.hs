module ParsingSpec where

import Parsing (normParse)
import Data.Either (isRight)

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck ( chooseInt, sublistOf, forAll, Gen )
import Types ( depwinode, genSynTree )
import Parsing ( normParse )
import Data.Char (isLetter)
import qualified Control.Exception as Exc (evaluate)
import Data.Maybe ( fromJust, isNothing )

spec :: Spec
spec =
  describe "normParse" $
    it "correctly recognizes simple double negation" $
      isRight $ normParse "~~A"
