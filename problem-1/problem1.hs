-- PART 1

inputs :: IO [Integer]
inputs = map read . lines <$> readFile "input.txt"

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

-- ENTRY POINT

main :: IO ()
main = do
    putStr "Part 1: "
    print =<< part1
    putStr "Part 2: "
    print =<< part2
    