module Auxiliary (
    listNoDuplicate
) where

import Data.List.Extra(nubOrd)

listNoDuplicate :: Ord a => [a] -> Bool
listNoDuplicate list = nubOrd list == list
