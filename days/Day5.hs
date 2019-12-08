module Day5 (day5) where

import IntCode   (Computer (_outputs), execComputer, programToComputer,
                  supplyInputs)
import Puzzle    (puzzle)
import ReadInput (inputPath, readProgram)

import Data.List (last)

runTEST :: Int -> IO Int
runTEST x = do
    program <- readProgram $(inputPath)
    return . last . _outputs . execComputer . supplyInputs [x] . programToComputer $ program

part1, part2 :: IO Int
part1 = runTEST 1
part2 = runTEST 5

day5 :: IO ()
day5 = puzzle part1 part2
