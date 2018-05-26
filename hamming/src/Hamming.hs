module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys | length xs /= length ys = Nothing
               | otherwise = Just (d xs ys)
               where d [] [] = 0
                     d (x:xs) (y:ys) = if x /= y then 1 + d xs ys else 0 + d xs ys 
