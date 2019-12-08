module Day1 (day1) where

import Puzzle   (puzzle)
import ReadInput (readInts)

inputs :: IO [Int]
inputs = readInts "input1.txt"

-- PART 1

fuelMass :: Int -> Int
fuelMass x = (x `div` 3) - 2

part1 :: IO Int
part1 = sum . map fuelMass <$> inputs

-- PART 2

moduleMass :: Int -> Int
moduleMass x
    | m <= 0 = 0
    | otherwise = m + moduleMass m
        where
            m = fuelMass x

part2 :: IO Int
part2 = sum . map moduleMass <$> inputs

-- EXPORTED SOLUTION

day1 :: IO ()
day1 = puzzle 1 part1 part2
