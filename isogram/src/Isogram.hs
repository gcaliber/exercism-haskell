module Isogram (isIsogram) where

import Data.Char (toLower)

isIsogram :: String -> Bool
isIsogram [] = True
isIsogram xs = lx `notElem` lxs && isIsogram (tail xs)
             where (lx:lxs) = filter (`elem` ['a'..'z']) (map toLower xs) 