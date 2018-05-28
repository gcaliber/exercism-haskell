module RunLength (decode, encode) where

import Data.List (group)
import Data.Char (isAlpha, isDigit)

decode :: String -> String
decode [] = []
decode (x:xs) | length (x:xs) == 1 = [x]
              | not $ isDigit x = x : decode xs
              | otherwise = replicate count (head rest) ++ decode (tail rest)
              where count = read $ takeWhile isDigit (x:xs)
                    rest  = dropWhile isDigit (x:xs)

encode :: String -> String
encode xs = concatMap charCounts (group xs)
          where charCounts ys | length ys == 1 = [head ys]
                              | otherwise = show (length ys) ++ [head ys]