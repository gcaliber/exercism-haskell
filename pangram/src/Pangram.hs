module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (sort, nub)

isPangram :: String -> Bool
isPangram xs = (sort . nub) (filter (`elem` ['a'..'z']) (map toLower xs)) == ['a'..'z']