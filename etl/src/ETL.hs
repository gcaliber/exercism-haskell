module ETL (transform) where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

transform :: Map a String -> Map Char a
transform legacyData = Map.fromList $ concatMap extract (Map.toList legacyData)
                     where extract (k, vs) = map (\v -> (toLower v, k)) vs

