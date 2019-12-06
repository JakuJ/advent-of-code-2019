{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy (Text, pack, unpack, splitOn)
import Data.Bifunctor (bimap)

type Vector = (Int, Int) -- (change in X direction, change in Y direction)
type Path = [Vector]

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

splitByCommas :: String -> [String]
splitByCommas = map unpack . splitOn "," . pack

parseVector :: String -> Vector
parseVector (x:xs) = both (* len) direction
    where
        len = read xs
        direction = case x of
            'U' -> (0, 1)
            'D' -> (0, -1)
            'L' -> (-1, 0)
            'R' -> (1, 0)
            _ -> error "Invalid direction"

type Point = (Int, Int)

data Segment = Segment { start :: Point, end :: Point }

instance Show Segment where
    show (Segment p1 p2) = show p1 ++ " -> " ++ show p2 

type Wire = [Segment]

pathToWire :: Path -> Wire
pathToWire = splitPairs . scanl vecAdd (0, 0)
    where
        vecAdd (x, y) (z, w) = (x + z, y + w)
        splitPairs [p1, p2] = [Segment p1 p2]
        splitPairs (p1:p2:ps) = Segment p1 p2 : splitPairs (p2 : ps)

inputs :: IO (Wire, Wire)
inputs = do
    [first, second] <- map splitByCommas . lines <$> readFile "input.txt"
    return $ both (pathToWire . map parseVector) (first, second)

-- PART 1

-- O (1)
vertical :: Segment -> Bool
vertical (Segment s e) = fst s == fst e

-- O (1)
horizontal :: Segment -> Bool
horizontal = not vertical

-- O (1)
splitVH :: Segment -> Segment -> (Segment, Segment)
splitVH s1 s2 = if vertical s1 then (s1, s2) else (s2, s1) 

-- cross :: (Segment, Segment) -> Maybe Point
-- cross s1@(p1, p2) s2@(p3, p4)
--     | 

intersect :: Wire -> Wire -> Maybe Point
intersect s1 s2
    | vertical s1 && vertical s2 = Nothing
    | horizontal s1 && horizontal s2 = Nothing
    | otherwise = cross $ splitVH s1 s2

-- O (n^2)
intersections :: (Wire, Wire) -> [Point]
intersections w1 w2 = do
    seg1 <- w1
    seg2 <- w2
    guard ()

-- bruteforce :: (Wire, Wire) -> Point
-- bruteforce = let (x, y, _) = sortBy closest intersctions in (x, y)
        

main :: IO ()
main = do
    wires <- inputs
    print $ both length wires