module DNA (nucleotideCounts) where

import qualified Data.Map as M

emptyNucleotideMap = M.fromList [ ('A', 0), ('C', 0), ('G', 0), ('T', 0) ]

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts [] = Right emptyNucleotideMap
nucleotideCounts xs | any (`notElem` "ACGT") xs = Left "Invalid nucleotide"
                    | otherwise = Right (foldl (\m x -> M.insertWith (+) x 1 m) emptyNucleotideMap xs)