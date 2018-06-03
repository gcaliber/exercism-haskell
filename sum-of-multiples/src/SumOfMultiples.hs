module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub $ foldr (\ x -> (++) [x, x + x .. limit - 1]) [] factors