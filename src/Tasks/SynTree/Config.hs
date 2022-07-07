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
  , mustcontain :: Int
  ,addoper::Bool
  } deriving Show

dSynTreeConfig :: SynTreeConfig
dSynTreeConfig =
  SynTreeConfig
  { maxnode = 10
  , minnode = 6
  , maxdepth = 6
  , electliteral = "ABCDE"
  , mustcontain = 3
  ,addoper = False
  }

data SynTreeInst =
  SynTreeInst
  { insSyntree :: SynTree Char
  , image :: String
  , correct :: String
  } deriving Show
