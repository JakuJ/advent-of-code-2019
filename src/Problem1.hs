module Problem1 (problem1) where

import Problem   (problem)
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

problem1 :: IO ()
problem1 = problem 1 part1 part2
