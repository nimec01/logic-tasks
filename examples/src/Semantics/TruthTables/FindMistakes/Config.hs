module Semantics.TruthTables.FindMistakes.Config where

import LogicTasks.Config (
  BaseConfig(..),
  DecideConfig(..),
  NormalFormConfig(..),
  FormulaConfig(..)
  )
import Control.OutputCapable.Blocks (Language (..))
import Test.Hspec
import Util.VerifyConfig
import qualified Data.Map as Map (fromList)
import Data.Map (Map)

listToFM :: Ord k => [(k, a)] -> Map k a
listToFM = Map.fromList

-- Weight 0.34
task09 :: DecideConfig
task09 = DecideConfig
  { formulaConfig =
      FormulaCnf (NormalFormConfig
                   { baseConf = BaseConfig
                     { minClauseLength = 2
                     , maxClauseLength = 2
                     , usedLiterals = "ABCD"
                     }
                   , minClauseAmount = 3
                   , maxClauseAmount = 3
                   })
  , percentageOfChanged = 40
  , percentTrueEntries = Nothing
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.4
task11 :: DecideConfig
task11 = DecideConfig
  { formulaConfig =
      FormulaCnf (NormalFormConfig
                   { baseConf = BaseConfig
                     { minClauseLength = 2
                     , maxClauseLength = 3
                     , usedLiterals = "ABCD"
                     }
                   , minClauseAmount = 4
                   , maxClauseAmount = 4
                   })
  , percentageOfChanged = 40
  , percentTrueEntries = Nothing
  , extraText = Just (listToFM
                       [(German, "Sie haben nur 2 Versuche, die Aufgabe zu lösen."),
                        (English, "You have only 2 attempts to solve this task.")
                       ])
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task09" $ verifyFormulaConfig (formulaConfig task09)
  describe "task11" $ verifyFormulaConfig (formulaConfig task11)
