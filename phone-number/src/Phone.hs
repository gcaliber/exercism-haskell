module Phone (number) where

number :: String -> Maybe String
number xs = if isValid xs then Just (clean xs) else Nothing
          where clean xs = let c = filter (`elem` ['0'..'9']) xs
                           in if length c == 10 then c else tail c

isValid :: String -> Bool
isValid xs | any (`notElem` ['0'..'9'] ++ "+ .-()") xs = False
           | any (`notElem` ['0'..'9']) xs = isValid $ filter (`elem` ['0'..'9']) xs
           | length xs > 11 = False
           | length xs < 10 = False
           | length xs == 11 && head xs /= '1' = False
           | length xs == 11 && head xs == '1' = isValid (tail xs)
           | head xs `elem` ['2'..'9'] && xs !! 3 `elem` ['2'..'9'] = True
           | otherwise = False