{-# LANGUAGE TypeOperators #-}
module TestHelpers (
  deleteBrackets,
  deleteSpaces,
  doesNotRefuse
  ) where

import Data.Char (isSpace)
import Control.OutputCapable.Blocks.Generic (evalLangM, RunnableOutputCapable (RunMonad), GenericLangM)
import Control.Monad.Identity (Identity(runIdentity))
import Data.Maybe (isJust)

deleteBrackets :: String  -> String
deleteBrackets = filter (`notElem` "()")

deleteSpaces :: String  -> String
deleteSpaces = filter (not . isSpace)

doesNotRefuse :: (RunMonad l m ~ Identity,  RunnableOutputCapable l m) => GenericLangM l m a -> Bool
doesNotRefuse langM = isJust (runIdentity (evalLangM langM))
