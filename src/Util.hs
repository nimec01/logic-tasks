{-# language RecordWildCards #-}

module Util where


import Config (BaseConfig(..), CnfConfig(..))

import Control.Monad.Output




prevent :: OutputMonad m => Bool -> LangM m -> LangM m
prevent b = assertion $ not b


preventWithHint :: OutputMonad m => Bool -> LangM m -> LangM m -> LangM m
preventWithHint b desc hint = do
  yesNo (not b) desc
  if b
    then refuse $ indent hint
    else pure()


pairwiseCheck :: Eq a => [(a,a,Int)] -> ([Int],[Int])
pairwiseCheck [] = ([],[])
pairwiseCheck ((x,y,index):xs)
    | x == y = (index:same,diff)
    | otherwise = (same,index:diff)
  where (same,diff) = pairwiseCheck xs




isOutside :: Int -> Int -> Int -> Bool
isOutside lower upper x = x < lower || x > upper





checkBaseConf :: OutputMonad m => BaseConfig -> LangM m
checkBaseConf BaseConfig{..}
    | any (<1) [minClauseLength, maxClauseLength] =
        refuse $ indent $ translate $ do
          english "At least one length parameter is negative."
          german "Mindestens eines der 'length'-Parameter ist negativ."

    | minClauseLength > maxClauseLength =
        refuse $ indent $ translate $ do
          english "The minimum clause length is greater than the maximum clause length."
          german "Die untere Grenze der Klausellänge ist höher als die obere."


    | length usedLiterals < minClauseLength =
        refuse $ indent $ translate $ do
          german "Zu wenige Literale für diese Klausellänge."
          english "There are not enough literals available for this clause length."

    | null usedLiterals =
        refuse $ indent $ translate $ do
          german "Es wurden keine Literale angegeben."
          english "You did not specify which literals should be used."

    | otherwise = pure ()



checkCnfConf :: OutputMonad m => CnfConfig -> LangM m
checkCnfConf CnfConfig {..}
    | any (<1) [minClauseAmount, maxClauseAmount] =
        refuse $ indent $ translate $ do
          german "Mindestens eines der 'amount'-Parater ist negativ."
          english "At least one amount parameter is negative."

    | minClauseAmount > maxClauseAmount =
        refuse $ indent $ translate $ do
          german "Die untere Grenze der Klauselanzahl ist höher als die obere."
          english "The minimum amount of clauses is greater than the maximum amount."

    | minClauseAmount > minimum [2^maxClauseLength baseConf, 2^length (usedLiterals baseConf)] =
        refuse $ indent $ translate $ do
          german "Mit diesen Einstellungen kann die gewünschte Anzahl und Länge an Klauseln nicht erfüllt werden."
          english "There are not enough combinations available to satisfy your amount and length settings."

    | otherwise = checkBaseConf baseConf
