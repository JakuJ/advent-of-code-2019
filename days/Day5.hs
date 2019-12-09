module Day5 (part1, part2) where

import IntCode   (Computer (_outputs), execComputer, programToComputer,
                  supplyInputs)
import ReadInput (inputPath, readProgram)

import Data.List (last)

runTEST :: Integer -> IO Integer
runTEST x = do
    program <- readProgram $(inputPath)
    return . last . _outputs . execComputer . supplyInputs [x] . programToComputer $ program

part1, part2 :: IO Integer
part1 = runTEST 1
part2 = runTEST 5
