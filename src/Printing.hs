{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Printing
       ( showIndexedList
       , myText
       , Print(..)
       ) where


import Config
import Types

import Data.List (intersperse)
import Data.Text.Lazy (pack)


import Text.PrettyPrint.Leijen.Text





myText :: String -> Doc
myText = text . pack


class Print a where
  printer :: a -> Doc


instance Print a => Print [a] where
  printer xs = hcat [ char '[', innerPrint xs, char ']']
    where
      innerPrint ls = hsep $ intersperse (char ',') $ map printer ls


instance Print Number where
  printer num = maybe empty pretty $ value num




instance Print Literal where
   printer (Literal x) = char x
   printer (Not x) = myText ['~', x]



instance Print Clause where
    printer clause = listShow $ literals clause
      where
        listShow [] = empty
        listShow [x] = printer x
        listShow (x:xs) = hsep [printer x, text "\\/", listShow xs]



instance Print Cnf where
    printer cnf = listShow $ getClauses cnf
      where
        listShow [] = empty
        listShow [x] = hcat [char '(', printer x, char ')']
        listShow (x:xs) = hsep
                           [ hcat [char '(', printer x, char ')']
                           , text "/\\"
                           , listShow xs
                           ]




instance Print Table where
  printer t = myText (show t)




instance Print PickInst where
  printer  PickInst{..} =
      text "PickInst(" <> vcat
                           [ nest 2 $ printer cnfs
                           , char ',' <+> pretty correct
                           , maybe empty (\s -> myText (", {" ++ s ++ "}")) addText
                           , char ')'
                           ]









-- show tables side by side
showIndexedList :: Show b => Int -> Int -> [b] -> Doc
showIndexedList _ _ []= empty
showIndexedList maxLine gapSize xs = prodLines indexed
  where
    strings = map (lines . show) xs
    lineLen = length $ head $ head strings
    indices = [show num ++ replicate (lineLen - length (show num)) ' ' | num <- [1..length xs]]
    indexed = zipWith (:) indices strings
    gaps = replicate gapSize ' '
    sperseGaps = replicate (length $ head indexed) gaps
    perLine = maxLine `div` (lineLen + gapSize)

    prodLines ys
        | null ys = empty
        | perLine <= length ys = vcat [ vcat (map myText (docRow tRow))
                                      , line
                                      , prodLines rest
                                      ]
        | otherwise = vcat (map myText (docRow ys))
      where
        tRow = take perLine ys
        rest = drop perLine ys
        docRow ls = foldl1 (zipWith (++)) (intersperse sperseGaps ls)


