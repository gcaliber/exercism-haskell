module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n | n < 1 = Nothing
           | n > aliquotSum  = Just Deficient
           | n == aliquotSum = Just Perfect
           | n < aliquotSum  = Just Abundant
           where aliquotSum = sum (filter (\x -> n `mod` x == 0) [1..n - 1])