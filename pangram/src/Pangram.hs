module Pangram (isPangram) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram xs = sort (nub (filter (not . (`elem` nonAlpha)) (map toLower xs))) == ['a'..'z']
    where nonAlpha = "`~!@#$%^&*()-_=+{}[]\\|;:\'\"<>,./?1234567890 \t\n\r\f\v"