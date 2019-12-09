module Day6 (part1, part2) where

import           ReadInput  (inputPath, readLines)

import qualified Data.Map   as Map
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Tuple (swap)

type Edge = (String, String)
type OrbitMap = Map.Map String [String]

parse :: String -> Edge
parse str = (take 3 str, drop 4 str)

orbitMap :: [Edge] -> OrbitMap
orbitMap = foldl (\m (from, to) -> Map.insertWith (++) from [to] m) Map.empty

collect :: String -> Int -> OrbitMap -> Int
collect current passed m
    | null neighbors = orbits
    | otherwise = orbits + (sum . map (\next -> collect next (passed + 1) m)) neighbors
        where
            neighbors = Map.findWithDefault [] current m
            orbits = 1 + indirect
            indirect = max 0 (passed - 1)

-- PART 1

part1 :: IO Int
part1 = do
    orbits <- readLines $(inputPath)
    let m = orbitMap $ map parse orbits
    return $ collect "COM" 0 m - 1

twoWayMap :: [Edge] -> OrbitMap
twoWayMap edges = Map.unionWith (++) outwardMap inverseMap
    where
        outwardMap = orbitMap edges
        inverseMap = orbitMap $ map swap edges

treePath :: String -> String -> Int -> OrbitMap -> Maybe Int
treePath current previous total m
    | null neighbors = case current of
        "SAN" -> Just $ total - 2
        _     -> Nothing
    | otherwise = case results of
        []  -> Nothing
        [x] -> Just x
        where
            results = mapMaybe (\next -> treePath next current (total + 1) m) neighbors
            neighbors = filter (/= previous) $ Map.findWithDefault [] current m

-- PART 2

part2 :: IO Int
part2 = do
    orbits <- readLines $(inputPath)
    let m = twoWayMap $ map parse orbits
    return . fromJust $ treePath "YOU" "" 0 m
