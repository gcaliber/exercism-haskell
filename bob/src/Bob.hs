module Bob (responseFor) where

import Data.Char

sure     = "Sure."
chill    = "Whoa, chill out!"
calm     = "Calm down, I know what I'm doing!"
fine     = "Fine. Be that way!"
whatever = "Whatever."

responseFor :: String -> String
responseFor [] = fine
responseFor "?" = sure
responseFor xs | any (`elem` unwanted) xs = responseFor (filter (not . (`elem` unwanted)) xs)
               | all isNumber xs        = whatever
               | all isNumber (init xs) = sure
               | any isLower xs         = if last xs == '?' then sure else whatever
               | otherwise              = if last xs == '?' then calm else chill
               where 
                    unwanted = "`~!@#$%^&*()-_=+[]{}\\|;:\'\"<>,./ \t\n\r\f\v"