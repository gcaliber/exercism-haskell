module Pangram (isPangram) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram xs = sort (nub (filter (`elem` ['a'..'z']) (map toLower xs))) == ['a'..'z']