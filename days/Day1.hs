module Day1 (part1, part2) where

import ReadInput (inputPath, readInts)

inputs :: IO [Int]
inputs = readInts $(inputPath)

fuelMass :: Int -> Int
fuelMass x = (x `div` 3) - 2

-- PART 1

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
