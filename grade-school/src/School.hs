module School (School, add, empty, grade, sorted) where

import qualified Data.Map as Map
import Data.List (sort)
import Data.Map (Map, insertWith, lookup)
import Data.Maybe (fromMaybe)

type School = Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student = Map.insertWith (++) gradeNum [student]

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = sort (fromMaybe [] (Map.lookup gradeNum school))

sorted :: School -> [(Int, [String])]
sorted school = Map.toAscList (Map.map sort school)