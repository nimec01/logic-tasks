module Syntax.TreeToFormula.Config where

import Test.Hspec

import Tasks.SynTree.Config (
  SynTreeConfig(..)
  )
import Trees.Types (BinOp(..))
import Tasks.TreeToFormula.Config (
  TreeToFormulaConfig(..),checkTreeToFormulaConfig,
  )
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))
import qualified Data.Map as Map (fromList)
import Data.Map (Map)

listToFM :: Ord k => [(k, a)] -> Map k a
listToFM = Map.fromList

-- 2024: Weight 0.34
task02 :: TreeToFormulaConfig
task02 = TreeToFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 10
    , maxNodes = 12
    , minDepth = 4
    , maxDepth = 5
    , availableAtoms = "ABCDE"
    , minAmountOfUniqueAtoms = 5
    , binOpFrequencies = listToFM
      [ (And, 1)
      , (Or, 1)
      , (Impl, 0)
      , (BackImpl, 0)
      , (Equi, 0)
      ]
    , negOpFrequency = 1
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 2
    }
  , extraHintsOnSemanticEquivalence = True
  , extraHintsOnAssociativity = True
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = True
  }

-- Weight 0.33
task04 :: TreeToFormulaConfig
task04 =  TreeToFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 15
    , maxNodes = 18
    , minDepth = 4
    , maxDepth = 9
    , availableAtoms = "ABCDEFG"
    , minAmountOfUniqueAtoms = 7
    , binOpFrequencies = listToFM
      [ (And, 1)
      , (Or, 1)
      , (Impl, 0)
      , (BackImpl, 0)
      , (Equi, 0)
      ]
    , negOpFrequency = 1
    , maxConsecutiveNegations = 3
    , minUniqueBinOperators = 2
    }
  , extraHintsOnSemanticEquivalence = True
  , extraHintsOnAssociativity = True
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

-- Weight 0.4
task10 :: TreeToFormulaConfig
task10 = task04

spec :: Spec
spec = do
  describe "task02" $ verifyConfig German task02 checkTreeToFormulaConfig
  describe "task04" $ verifyConfig German task04 checkTreeToFormulaConfig
