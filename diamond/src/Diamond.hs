module Diamond (diamond) where

import Data.Char

diamond :: Char -> Maybe [String]
diamond c | not (isAlpha c) = Nothing
          | otherwise = Just (d ++ tail (reverse d))
          where d = diamond' ['A'..c]

diamond' :: String -> [String]
diamond' [] = [] 
diamond' (x:xs) | x == 'A'  = (extSpaces ++ [x] ++ extSpaces) : diamond' xs
                | otherwise = (extSpaces ++ [x] ++ inSpaces ++ [x] ++ extSpaces) : diamond' xs
                where extSpaces = replicate (length xs) ' '
                      inSpaces  = replicate (1 + 2 * (fromEnum x - 66)) ' '