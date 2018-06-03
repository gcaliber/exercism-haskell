module Raindrops (convert) where

drops = [(3, "Pling"), (5, "Plang"), (7, "Plong")]

convert :: Int -> String
convert n = if null drips then show n else drips 
          where drips = concatMap snd (filter (\ (k, _) -> n `mod` k == 0) drops)