module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA [] = Nothing
toRNA xs | any (`notElem` "GCTA") xs = Nothing 
         | otherwise = Just (map toRNAchar xs)

toRNAchar :: Char -> Char
toRNAchar 'G' = 'C'
toRNAchar 'C' = 'G'
toRNAchar 'T' = 'A'
toRNAchar 'A' = 'U'