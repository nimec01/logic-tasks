module TestHelpers (
  deleteBrackets,
  deleteSpaces,
  transferSetIntToString,
  ) where

import Data.Char (isSpace)
import Data.Set (toList, Set)
import Data.List (intercalate)

deleteBrackets :: String  -> String
deleteBrackets = filter (`notElem` "()")

deleteSpaces :: String  -> String
deleteSpaces = filter (not . isSpace)

transferSetIntToString :: Set Int -> String
transferSetIntToString setInt = "{" ++ intercalate "," (map show (toList setInt)) ++ "}"
