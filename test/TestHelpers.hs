module TestHelpers (
  deleteBrackets,
  deleteSpaces,
  ) where

import Data.Char (isSpace)

deleteBrackets :: String  -> String
deleteBrackets = filter (`notElem` "()")

deleteSpaces :: String  -> String
deleteSpaces = filter (not . isSpace)
