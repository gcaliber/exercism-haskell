module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n | n < 1     = Nothing
          | otherwise = Just (collatz' n)

collatz' :: Integer -> Integer
collatz' 1 = 0
collatz' n | n `mod` 2 == 0 = 1 + collatz' (n `div` 2)             
           | otherwise      = 1 + collatz' (n * 3 + 1)