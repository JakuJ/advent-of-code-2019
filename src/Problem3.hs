{-# LANGUAGE OverloadedStrings #-}

module Problem3 (problem3) where

import ReadInput (readCSV)

import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes)

type Vector = (Int, Int) -- (change in X direction, change in Y direction)
type Path = [Vector]

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

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
type Segment = (Point, Point)
type Wire = [Segment]

pathToWire :: Path -> Wire
pathToWire = splitPairs . scanl vecAdd (0, 0)
    where
        vecAdd (x, y) (z, w) = (x + z, y + w)
        splitPairs :: [Point] -> [Segment]
        splitPairs [p1, p2] = [(p1, p2)]
        splitPairs (p1:p2:ps) = (p1, p2) : splitPairs (p2 : ps)

inputs :: IO (Wire, Wire)
inputs = do
    [first, second] <- readCSV "input3.txt"
    return $ both (pathToWire . map parseVector) (first, second)

-- PART 1

getX, getY :: Point -> Int
getX = fst
getY = snd

-- O(1)
vertical :: Segment -> Bool
vertical (s, e) = getX s == getX e

-- O(1)
horizontal :: Segment -> Bool
horizontal = not . vertical

-- O(1)
splitVH :: Segment -> Segment -> (Segment, Segment)
splitVH s1 s2 = bimap vsort hsort (v, h)
    where
        (v, h) = if vertical s1 then (s1, s2) else (s2, s1)
        vsort (p1, p2) = if getY p1 < getY p2 then (p1, p2) else (p2, p1)
        hsort (p1, p2) = if getX p1 < getX p2 then (p1, p2) else (p2, p1)

-- O(1)
cross :: (Segment, Segment) -> Maybe Point
cross (v@(v1, v2), h@(h1, h2))
    | getY v1 > hy = Nothing
    | getY v2 < hy = Nothing
    | getX h1 > vx = Nothing
    | getX h2 < vx = Nothing
    | otherwise = Just (vx, hy)
        where
            hy = getY h1
            vx = getX v1

-- O(1)
intersect :: Segment -> Segment -> Maybe Point
intersect s1 s2
    | vertical s1 && vertical s2 = Nothing
    | horizontal s1 && horizontal s2 = Nothing
    | otherwise = cross $ splitVH s1 s2

-- O (n^2)
intersections :: Wire -> Wire -> [Point]
intersections w1 w2 = catMaybes $ do
    seg1 <- w1
    intersect seg1 <$> w2

-- O(1)
manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

-- O(n^2)
part1 :: IO Int
part1 = minimum . map manhattan . uncurry intersections <$> inputs

-- EXPORTED SOLUTION

problem3 :: IO ()
problem3 = do
    putStrLn "Problem 3:"
    putStr "\tPart 1: "
    print =<< part1