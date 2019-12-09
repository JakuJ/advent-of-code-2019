module Main where

import qualified Day1   (part1, part2)
import qualified Day2   (part1, part2)
import qualified Day3   (part1, part2)
import qualified Day4   (part1, part2)
import qualified Day5   (part1, part2)
import qualified Day6   (part1, part2)
import qualified Day7   (part1, part2)
import qualified Day8   (part1, part2)
import qualified Day9   (part1, part2)

import           DaysTH (runAllDays)

main :: IO ()
main = $(runAllDays)
