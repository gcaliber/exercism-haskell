module Acronym (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate [] = []
abbreviate (x:xs) = toUpper x : abbr xs
    where abbr (x:y:ys) | x `elem` " -"  = toUpper y : abbr ys
                        | not (isUpper x) && isUpper y = y : abbr ys
                        | otherwise = abbr (y:ys)
          abbr _ = []