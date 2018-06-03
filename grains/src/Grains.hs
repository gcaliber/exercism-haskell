module Grains (square, total) where

square :: Integer -> Maybe Integer
square n | n < 1     = Nothing
         | n > 64    = Nothing
         | otherwise = Just (square' n)

square' :: Integer -> Integer
square' 1 = 1
square' n = 2 * square' (n - 1)

total :: Integer
total = sum $ map square' [1..64]