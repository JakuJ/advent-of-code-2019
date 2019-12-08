module Day3 (day3) where

import           Puzzle                   (puzzle)
import           ReadInput                (inputPath, readCSV)

import           Control.Monad.State.Lazy (State, evalState, get, modify)
import           Data.Bifunctor           (bimap)
import           Data.Function            (on)
import           Data.List                (sortOn)
import qualified Data.Map                 as Map
import           Data.Maybe               (catMaybes, mapMaybe)

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
            _   -> error "Invalid direction"

type Point = (Int, Int)
type Segment = (Point, Point)
type Wire = [Segment]

vecAdd, vecSub :: Num a => (a, a) -> (a, a) -> (a, a)
vecAdd (a, b) (c, d) = (a + c, b + d)
vecSub (a, b) (c, d) = (a - c, b - d)

pathToWire :: Path -> Wire
pathToWire = splitPairs . scanl vecAdd (0, 0)
    where
        splitPairs :: [Point] -> [Segment]
        splitPairs [p1, p2]   = [(p1, p2)]
        splitPairs (p1:p2:ps) = (p1, p2) : splitPairs (p2 : ps)

inputs :: IO (Path, Path)
inputs = do
    [first, second] <- readCSV $(inputPath)
    return $ both (map parseVector) (first, second)

-- PART 1

getX, getY :: Point -> Int
getX = fst
getY = snd

-- O(1)
vertical, horizontal :: Segment -> Bool
vertical (s, e) = getX s == getX e
horizontal (s, e) = getY s == getY e

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

-- O (n ^ 2)
intersections :: Wire -> Wire -> [Point]
intersections w1 w2 = catMaybes $ do
    seg1 <- w1
    intersect seg1 <$> w2

-- O(1)
manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

-- O(n ^ 2)
part1 :: IO Int
part1 = minimum . map manhattan . uncurry intersections . both pathToWire <$> inputs

-- PART 2

type DistanceMap = Map.Map Point Int -- Distance to intersection by both cables

-- O(n * log(n))
constructMap :: [Point] -> DistanceMap
constructMap = foldl (\acc p -> Map.insert p 0 acc) Map.empty

-- O(n * log(n))
walkPoints :: Point -> Int -> DistanceMap -> Path -> DistanceMap
walkPoints _ _ m [] = m
walkPoints p total m (v:vecs) = walkPoints nextPoint newTotal updatedMap vecs
    where
        nextPoint = p `vecAdd` v
        newTotal = total + manhattan v
        updatedMap = Map.adjust (const newTotal) nextPoint m

-- O(n * log(n))
pathToDists :: DistanceMap -> Path -> DistanceMap
pathToDists = walkPoints (0, 0) 0

-- O(1)
verticalV, horizontalV :: Vector -> Bool
verticalV (s, _) = s == 0
horizontalV (_, e) = e == 0

-- O(1)
splitsSegment :: Point -> Vector -> Point -> Bool
splitsSegment start long inter
    | not (verticalV short || horizontalV short) = False -- short not axis aligned
    | verticalV short == horizontalV long = False -- segments in different positions
    | verticalV short = on sameSign getY long short && on (<) (abs . getY) short long
    | horizontalV short = on sameSign getX long short && on (<) (abs . getX) short long
    where
        short :: Vector
        short = inter `vecSub` start
        sameSign :: Int -> Int -> Bool
        sameSign = (==) `on` signum

-- O(1)
segmentSplit :: Point -> Vector -> Point -> Maybe Vector
segmentSplit start vec pt
    | splitsSegment start vec pt = Just dist1
    | otherwise = Nothing
        where
            dist1 = pt `vecSub` start

-- O(n * m), m - number of intersections
trySplit :: Path -> [Point] -> State Point Path
trySplit [] _ = return []
trySplit (v1:vecs) inters = do
    start <- get
    let splits = mapMaybe (segmentSplit start v1) inters
    let cutDistances = sortOn manhattan (v1 : splits)
    let (nextStep : nextSteps) = cut cutDistances
    modify (vecAdd nextStep)
    rest <- trySplit (nextSteps ++ vecs) inters
    return $ nextStep : rest
        where
            cut l@(x:xs) = x : zipWith vecSub xs l

-- O(n * m)
splitOnIntersections :: [Point] -> Path -> Path
splitOnIntersections inters path = evalState (trySplit path inters) (0, 0)

-- O(n ^ 2)
minimumSteps :: (Path, Path) -> Int
minimumSteps paths = minimum . Map.elems $ distMap
    where
        inters = filter (/= (0, 0)) . uncurry intersections . both pathToWire $ paths
        m0 = constructMap inters
        newPaths = both (splitOnIntersections inters) paths
        (m1, m2) = both (pathToDists m0) newPaths
        distMap = Map.intersectionWith (+) m1 m2

part2 :: IO Int
part2 = minimumSteps <$> inputs

-- EXPORTED SOLUTION

day3 :: IO ()
day3 = puzzle part1 part2
