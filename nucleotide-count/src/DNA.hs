module DNA (nucleotideCounts) where

import qualified Data.Map as M

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts xs | any (`notElem` "ACGT") xs = Left $ "Invalid nucleotide, contains invalid characters: " ++ xs
                    | otherwise = Right (foldl (\m x -> M.insertWith (+) x 1 m) emptyNucleotideMap xs)
                    where emptyNucleotideMap = M.fromList [('A', 0), ('C', 0), ('G', 0), ('T', 0)]
