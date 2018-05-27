module Isogram (isIsogram) where

import Data.Char

isIsogram :: String -> Bool
isIsogram [] = True
isIsogram (x:xs) | not (isAlpha x) = isIsogram xs
                 | otherwise = toLower x `notElem` xs && toUpper x `notElem` xs && isIsogram xs