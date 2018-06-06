module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = M.Map String [Plant]

garden :: [String] -> String -> Garden
garden students plants = garden' students (convertPlants p1 p2)
    where p1 = head (lines plants)
          p2 = last (lines plants)

garden' :: [String] -> [[Plant]] -> Garden
garden' _ []          = M.empty
garden' (x:xs) (y:ys) = M.insert x y (garden' xs ys)

convertPlants :: String -> String -> [[Plant]]
convertPlants [] [] = []
convertPlants xs ys = (map unabbrev (take 2 xs ++ take 2 ys)) : (convertPlants (drop 2 xs) (drop 2 ys))
    where unabbrev 'C' = Clover
          unabbrev 'G' = Grass
          unabbrev 'R' = Radishes
          unabbrev 'V' = Violets

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = fromMaybe [] (M.lookup student garden)


