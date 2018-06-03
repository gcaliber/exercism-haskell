module IsbnVerifier (isbn) where

import Data.Char (digitToInt)

isbn :: String -> Bool
isbn [] = False
isbn xs | length xs < 10 = False
        | any (`notElem` ['0'..'9'] ++ "-X") xs = False
        | 'X' `elem` init xs = False
        | length cleaned > 10 = False
        | otherwise = sum (zipWith (*) isbnInt [10,9..1]) `mod` 11 == 0
        where cleaned = filter (`elem` ['0'..'9'] ++ "X") xs
              isbnInt = map (\x -> if x == 'X' then 10 else digitToInt x) cleaned