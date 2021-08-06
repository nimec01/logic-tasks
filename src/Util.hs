{-# language RecordWildCards #-}

module Util where


import Config (BaseConfig(..), CnfConfig(..))
import Types





pairwiseCheck :: Eq a => [(a,a,Int)] -> ([Int],[Int])
pairwiseCheck [] = ([],[])
pairwiseCheck ((x,y,index):xs)
    | x == y = (index:same,diff)
    | otherwise = (same,index:diff)
  where (same,diff) = pairwiseCheck xs




isOutside :: Int -> Int -> Int -> Bool
isOutside lower upper x = x < lower || x > upper





checkBaseConf :: BaseConfig -> Maybe MText
checkBaseConf BaseConfig{..}
    | any (<1) [minClauseLength, maxClauseLength] =
        Just ("Mindestens eines der 'length'-Parameter ist negativ."
             ,"At least one length parameter is negative."
             )

    | minClauseLength > maxClauseLength =
        Just ("Die untere Grenze der Klausellänge ist höher als die obere."
             ,"The minimum clause length is greater than the maximum clause length."
             )

    | length usedLiterals < minClauseLength =
        Just ("Zu wenige Literale für diese Klausellänge."
             ,"There are not enough literals available for this clause length."
             )

    | null usedLiterals =
        Just ("Es wurden keine Literale angegeben."
             ,"You did not specify which literals should be used."
             )

    | otherwise = Nothing



checkCnfConf :: CnfConfig -> Maybe MText
checkCnfConf CnfConfig {..}
    | any (<1) [minClauseAmount, maxClauseAmount] =
        Just ("Mindestens eines der 'amount'-Parater ist negativ."
             ,"At least one amount parameter is negative."
             )

    | minClauseAmount > maxClauseAmount =
        Just ("Die untere Grenze der Klauselanzahl ist höher als die obere."
             ,"The minimum amount of clauses is greater than the maximum amount."
             )

    | minClauseAmount > minimum [2^maxClauseLength baseConf, 2^length (usedLiterals baseConf)] =
        Just ("Mit diesen Einstellungen kann die gewünschte Anzahl und Länge an Klauseln nicht erfüllt werden."
             ,"There are not enough combinations available to satisfy your amount and length settings."
             )

    | otherwise = checkBaseConf baseConf
