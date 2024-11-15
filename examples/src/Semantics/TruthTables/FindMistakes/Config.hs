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

-- 2024: Weight 0.3
task10 :: DecideConfig
task10 = DecideConfig
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
  , percentTrueEntries = Just (30,70)
  , printSolution = True
  , extraText = Nothing
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
                       [(German, "Sie haben nur 2 Versuche, die Aufgabe zu lösen."), {- german -}
                        (English, "You have only 2 attempts to solve this task.")
                       ])
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task10" $ verifyFormulaConfig (formulaConfig task10)
  describe "task11" $ verifyFormulaConfig (formulaConfig task11)
