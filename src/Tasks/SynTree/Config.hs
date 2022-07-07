module Tasks.SynTree.Config (
  SynTreeConfig(..),
  SynTreeInst(..),
  dSynTreeConfig,
  ) where

import Types (SynTree)

data SynTreeConfig =
  SynTreeConfig
  { maxnode :: Int
  , minnode :: Int
  , maxdepth :: Int
  , electliteral :: String
  , mustcontain :: String
  ,addoper::Bool
  } deriving Show

dSynTreeConfig :: SynTreeConfig
dSynTreeConfig =
  SynTreeConfig
  { maxnode = 10
  , minnode = 6
  , maxdepth = 6
  , electliteral = "ABCDE"
  , mustcontain = "ABC"
  ,addoper = False
  }

data SynTreeInst =
  SynTreeInst
  { insSyntree :: SynTree
  , image :: String
  , correct :: String
  } deriving Show
