module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA [] = Nothing
toRNA xs | filter (not . (`elem` "GCTA")) xs == [] = Just (toRNA' xs)
         | otherwise = Nothing

toRNA' :: String -> String
toRNA' []       = []
toRNA' ('G':xs) = 'C' : toRNA'(xs)
toRNA' ('C':xs) = 'G' : toRNA'(xs)
toRNA' ('T':xs) = 'A' : toRNA'(xs)
toRNA' ('A':xs) = 'U' : toRNA'(xs)
toRNA' xs       = [] 