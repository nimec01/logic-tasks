module Tasks.SynTree.Config (
  SynTreeConfig(..),
  SynTreeInst(..),
  dSynTreeConfig,
  ) where

import Types (SynTree)

data SynTreeConfig =
  SynTreeConfig
  { maxnode :: Integer
  , minnode :: Integer
  , maxdepth :: Integer
  , electliteral :: String
  , mustcontain :: String
  } deriving Show

dSynTreeConfig :: SynTreeConfig
dSynTreeConfig =
  SynTreeConfig
  { maxnode = 10
  , minnode = 6
  , maxdepth = 4
  , electliteral = "ABCDE"
  , mustcontain = "ABC"
  }

data SynTreeInst =
  SynTreeInst
  { insSyntree :: SynTree
  , image :: String
  , correct :: String
  } deriving Show
