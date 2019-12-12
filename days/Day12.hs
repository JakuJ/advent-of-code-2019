{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Day12 (part1, part2) where

import           Control.Lens
import           Data.Char      (isDigit)
import           Data.Function  (on)
import           Data.Hashable  (Hashable, hashWithSalt)
import qualified Data.HashSet   as HS
import           Data.List      (splitAt)
import           Data.Text.Lazy (Text, pack, splitOn, unpack)
import           Debug.Trace
import           GHC.Generics   (Generic)
import           ReadInput      (inputPath, readLines)

type Vector = (Int, Int, Int)

toVector :: [Int] -> Vector
toVector [a, b, c] = (a, b, c)

vPlus, vMinus :: Vector -> Vector -> Vector
vPlus a b = toVector $ zipWith (+) (a ^.. each) (b ^.. each)
vMinus a b = vPlus a (b & each %~ negate)

parsePos :: String -> Vector
parsePos = toVector . map (asInt . filterNum . unpack) . splitOn " " . pack
    where
        toVector [a, b, c] = (a, b, c)
        asInt x = read x :: Int
        filterNum = filter (\x -> isDigit x || x == '-')

data Planet = Planet {_position :: Vector, _velocity :: Vector}
    deriving (Show, Eq, Generic)

instance Hashable Planet

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

newtype System = System { unSystem :: (Planet, Planet, Planet, Planet) }
    deriving (Eq, Hashable)

toSystem [a, b, c, d] = System (a, b, c, d)

stepWith :: HS.HashSet System -> Int -> [Planet] -> Int
stepWith !set !n !pls
    | HS.member sys set = n
    | otherwise = stepWith (HS.insert sys set) (n + 1) (timeStep pls)
        where
            sys = toSystem pls

part2 :: IO Int
part2 = stepWith HS.empty 0 <$> getInput
