{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Day12 (part1, part2) where

import Control.Lens
import Data.Char      (isDigit)
import Data.Function  (on)
import Data.List      (splitAt)
import Data.Text.Lazy (Text, pack, splitOn, unpack)
import ReadInput      (inputPath, readLines)

type Vector = (Int, Int, Int)

vec3 :: [Int] -> Vector
vec3 [a, b, c] = (a, b, c)

vPlus, vMinus :: Vector -> Vector -> Vector
vPlus a b = vec3 $ zipWith (+) (a ^.. each) (b ^.. each)
vMinus a b = vPlus a (b & each %~ negate)

parsePos :: String -> Vector
parsePos = vec3 . map (asInt . filterNum . unpack) . splitOn " " . pack
    where
        vec3 [a, b, c] = (a, b, c)
        asInt x = read x :: Int
        filterNum = filter (\x -> isDigit x || x == '-')

data Planet = Planet {
                    _position :: {-# UNPACK #-} !Vector,
                    _velocity :: {-# UNPACK #-} !Vector
                    }
    deriving (Show, Eq)

makeLenses ''Planet

getInput :: IO [Planet]
getInput = map ((\p -> Planet p (0, 0, 0)) . parsePos) <$> readLines $(inputPath)

allButOne :: [a] -> [(a, [a])]
allButOne xs = do
    n <- [0 .. length xs - 1]
    let (ys, zs) = splitAt n xs in return (head zs, ys ++ tail zs)

applyGravity :: Planet -> [Planet] -> Planet
applyGravity pl pls = pl & velocity %~ vPlus (foldl1 vPlus gravities)
    where
        compare pos1 pos2 = pos2 `vMinus` pos1 & each %~ signum
        gravities = map ((compare `on` _position) pl) pls

applyGravities :: [Planet] -> [Planet]
applyGravities = map (uncurry applyGravity) . allButOne

applyVelocity :: Planet -> Planet
applyVelocity pl = let v = pl ^. velocity in pl & position %~ vPlus v

timeStep :: [Planet] -> [Planet]
timeStep = map applyVelocity . applyGravities

-- PART 1

potential, kinetic, total :: Planet -> Int
potential = sum . map abs . (^.. position . each)
kinetic = sum . map abs . (^.. velocity . each)
total pl = potential pl * kinetic pl

part1 :: IO Int
part1 = do
    planets <- getInput
    let _1000th = iterate timeStep planets !! 1000
    return $ sum $ map total _1000th

-- PART 2

type Axis = (Int, Int)
type System = (Axis, Axis, Axis, Axis)

vec4 [a, b, c, d] = (a, b, c, d)

toSystem :: _lens -> [Planet] -> System
toSystem sel pls = vec4 $ zip poss vels
    where
        poss = pls ^.. traverse . position . sel
        vels = pls ^.. traverse . velocity . sel

stepWith' :: _lens -> System -> Int -> [Planet] -> Int
stepWith' sel !init !n !pls
    | toSystem sel pls == init = n
    | otherwise = stepWith' sel init (n + 1) (timeStep pls)

stepWith :: _lens -> [Planet] -> Int
stepWith sel pls = stepWith' sel (toSystem sel pls) 1 (timeStep pls)

part2 :: IO Int
part2 = do
    planets <- getInput
    let xs = stepWith _1 planets
    let ys = stepWith _2 planets
    let zs = stepWith _3 planets
    return $ lcm xs (lcm ys zs)
