module TestHelpers (
  deleteBrackets
  ) where

deleteBrackets :: String  -> String
deleteBrackets = filter (`notElem` "()")
