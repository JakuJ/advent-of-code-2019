module Day10 (part1, part2) where

import ReadInput     (inputPath, readLines)

import Control.Lens
import Data.Function (on)
import Data.List     (delete, groupBy, maximum, maximumBy, sortOn)
import Data.Ord      (comparing)

type Point = (Int, Int)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn sel = groupBy (\a b -> sel a == sel b)

-- PARSING DATA

getMap :: IO [String]
getMap = readLines $(inputPath)

-- like Bytestring's elemIndices, I hope this gets stream-fused
toXs :: String -> [Int]
toXs = map fst . filter ((== '#') . snd) . zip [0..]

toPoints :: [String] -> [Point]
toPoints = concatMap zip' . (`zip` [0..]) . map toXs
    where
        zip' (xs, n) = zip xs (repeat n)

-- VECTOR MATH HELPERS

type Angle = Double
type Dist = Int

vSub :: Point -> Point -> Point
vSub (a, b) (c, d) = (a - c, b - d)

-- square root can be omitted
dist :: Point -> Point -> Dist
dist p1 p2 = let (x, y) = p2 `vSub` p1 in x * x + y * y

angleFrom :: Point -> Point -> Angle
angleFrom p1 p2 = uncurry (flip atan2 `on` fromIntegral) $ p2 `vSub` p1

toInfo :: Point -> Point -> (Angle, Dist, Point)
toInfo orig p = (angleFrom orig p, dist orig p, p)

-- PART 1

toGroups :: Point -> [Point] -> [(Angle, [(Dist, Point)])]
toGroups p pts = map extractAngle groups
    where
        extractAngle :: [(a, b, c)] -> (a, [(b, c)])
        extractAngle arr@(x:xs) = (x ^. _1, map (\(_, a, b) -> (a, b)) arr)
        groups :: [[(Angle, Dist, Point)]] -- sorted and grouped by angle
        groups = groupOn (^. _1) $ sortOn (^. _1) infos
        infos :: [(Angle, Dist, Point)]
        infos = map (toInfo p) (delete p pts)

visibilities :: [Point] -> [(Point, Int)]
visibilities pts = do
    p <- pts
    return (p, length (toGroups p pts))

part1 :: IO Int
part1 = maximum . map snd . visibilities . toPoints <$> getMap

-- PART 2

findCenter :: [Point] -> (Point, Int)
findCenter = maximumBy (comparing (^. _2)) . visibilities

distSort :: [(Angle, [(Dist, Point)])] -> [(Angle, [(Dist, Point)])]
distSort = traverse . _2 %~ sortOn fst

invY :: Point -> Point
invY = _2 %~ (39-)

part2 :: IO Int
part2 = do
    -- read inputs and find the laser's position
    pts <- toPoints <$> getMap
    let center = fst $ findCenter pts
    -- invert Y so that the angles make sense
    let invCenter = invY center
    let invPts = map invY pts
    -- group by angle from X axis and sort by distance
    let groups = distSort $ toGroups invCenter invPts
    -- convert the order as if it started from the Y axis
    let (q341, q2) = break ((> pi / 2) . (^. _1)) groups
    let ordered = reverse q341 ++ reverse q2
    -- find the 200th destroyed asteroid's position
    let _200th = invY $ ordered ^. singular (ix 199) . _2 . singular _head . _2
    -- compute the answer
    let (x, y) = _200th in return $ 100 * x + y
