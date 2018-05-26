module DNA (nucleotideCounts) where

import qualified Data.Map as M

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts [] = Right (M.fromList [ ('A', 0), ('C', 0), ('G', 0), ('T', 0) ])
nucleotideCounts xs | any (`notElem` "ACGT") xs = Left "Invalid nucleotide"
                    | otherwise = Right (snd (nc xs (M.fromList [ ('A', 0), ('C', 0), ('G', 0), ('T', 0) ])))

nc :: String -> M.Map Char Int -> (String, M.Map Char Int)
nc [] m   = ([], m)
nc (k:ks) m = nc ks (M.insertWith (+) k 1 m)