module Pangram (isPangram) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram xs = sort (nub (filter (`elem` alpha) (map toLower xs))) == alpha
    where alpha = ['a'..'z']