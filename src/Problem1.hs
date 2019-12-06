module Problem1 (problem1) where

import ReadInput (readIntegers)

inputs :: IO [Integer]
inputs = readIntegers "input1.txt"

-- PART 1

fuelMass :: Integer -> Integer
fuelMass x = (x `div` 3) - 2

part1 :: IO Integer
part1 = sum . map fuelMass <$> inputs

-- PART 2

moduleMass :: Integer -> Integer
moduleMass x
    | m <= 0 = 0
    | otherwise = m + moduleMass m
        where
            m = fuelMass x

part2 :: IO Integer
part2 = sum . map moduleMass <$> inputs

-- EXPORTED SOLUTION

problem1 :: IO ()
problem1 = do
    putStrLn "Problem 1:"
    putStr "\tPart 1: "
    print =<< part1
    putStr "\tPart 2: "
    print =<< part2
    