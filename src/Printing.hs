{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Printing
       ( showIndexedList
       , myText
       ) where


import Config
import Types

import Data.List (intersperse)
import Data.Text.Lazy (pack)


import Text.PrettyPrint.Leijen.Text





myText :: String -> Doc
myText = text . pack




instance Pretty Number where
  pretty num = maybe empty pretty $ value num



instance Pretty TruthValue where
  pretty tv = pretty $ truth tv


instance Pretty ResStep where
  pretty (Res (a,b,c)) = curlyBracesList (map curlyBracesList prep)
    where
      curlyBracesList = encloseSep (char '{') (char '}') (char ',')
      prep = map (map pretty) $ map literals [a,b,c]


instance Pretty Literal where
   pretty (Literal x) = char x
   pretty (Not x) = myText ['~', x]



instance Pretty Clause where
    pretty clause = listShow $ literals clause
      where
        listShow [] = empty
        listShow [x] = pretty x
        listShow (x:xs) = hsep [pretty x, text "\\/", listShow xs]



instance Pretty Cnf where
    pretty cnf = listShow $ getClauses cnf
      where
        listShow [] = empty
        listShow [x] = singlePrint x
        listShow (x:xs) = hsep
                           [ singlePrint x
                           , text "/\\"
                           , listShow xs
                           ]

        singlePrint x = if amount x == 1
                          then pretty x
                          else hcat [char '(', pretty x, char ')']




instance Pretty Table where
  pretty t = myText (show t)




instance Pretty PickInst where
  pretty  PickInst{..} =
      text "PickInst(" <> vcat
                           [ nest 2 $ pretty cnfs
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


