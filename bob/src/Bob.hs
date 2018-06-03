module Bob (responseFor) where

import Data.Char

sure     = "Sure."
chill    = "Whoa, chill out!"
calm     = "Calm down, I know what I'm doing!"
fine     = "Fine. Be that way!"
whatever = "Whatever."

responseFor :: String -> String
responseFor [] = fine
responseFor xs | any (`elem` ignored) xs = responseFor $ filter (not . (`elem` ignored)) xs
               | any isLower xs || all isNumber (init xs) = if last xs == '?' then sure else whatever
               | otherwise  = if last xs == '?' then calm else chill
               where ignored = "`~!@#$%^&*()-_=+[]{}\\|;:\'\"<>,./ \t\n\r\f\v"