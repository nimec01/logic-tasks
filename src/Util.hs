{-# language RecordWildCards #-}

module Util where


import Config (BaseConfig(..), CnfConfig(..))

import Control.Monad.Output





pairwiseCheck :: Eq a => [(a,a,Int)] -> ([Int],[Int])
pairwiseCheck [] = ([],[])
pairwiseCheck ((x,y,index):xs)
    | x == y = (index:same,diff)
    | otherwise = (same,index:diff)
  where (same,diff) = pairwiseCheck xs




isOutside :: Int -> Int -> Int -> Bool
isOutside lower upper x = x < lower || x > upper





checkBaseConf :: OutputMonad m => BaseConfig -> Maybe (LangM m)
checkBaseConf BaseConfig{..}
    | any (<1) [minClauseLength, maxClauseLength] =
        Just $ translate $ do
          english "At least one length parameter is negative."
          german "Mindestens eines der 'length'-Parameter ist negativ."

    | minClauseLength > maxClauseLength =
        Just $ translate $ do
          english "The minimum clause length is greater than the maximum clause length."
          german "Die untere Grenze der Klausellänge ist höher als die obere."


    | length usedLiterals < minClauseLength =
        Just $ translate $ do
          german "Zu wenige Literale für diese Klausellänge."
          english "There are not enough literals available for this clause length."

    | null usedLiterals =
        Just $ translate $ do
          german "Es wurden keine Literale angegeben."
          english "You did not specify which literals should be used."

    | otherwise = Nothing



checkCnfConf :: OutputMonad m => CnfConfig -> Maybe (LangM m)
checkCnfConf CnfConfig {..}
    | any (<1) [minClauseAmount, maxClauseAmount] =
        Just $ translate $ do
          german "Mindestens eines der 'amount'-Parater ist negativ."
          english "At least one amount parameter is negative."

    | minClauseAmount > maxClauseAmount =
        Just $ translate $ do
          german "Die untere Grenze der Klauselanzahl ist höher als die obere."
          english "The minimum amount of clauses is greater than the maximum amount."

    | minClauseAmount > minimum [2^maxClauseLength baseConf, 2^length (usedLiterals baseConf)] =
        Just $ translate $ do
          german "Mit diesen Einstellungen kann die gewünschte Anzahl und Länge an Klauseln nicht erfüllt werden."
          english "There are not enough combinations available to satisfy your amount and length settings."

    | otherwise = checkBaseConf baseConf
