module Problem5 (problem5) where

import Data.List (last)
import IntCode   (Computer (_outputs), execComputer, programToComputer,
                  supplyInputs)
import Problem   (problem)
import ReadInput (readProgram)

runTEST :: Int -> IO Int
runTEST x = do
    program <- readProgram "input5.txt"
    return . last . _outputs . execComputer . supplyInputs [x] . programToComputer $ program

part1, part2 :: IO Int
part1 = runTEST 1
part2 = runTEST 5

problem5 :: IO ()
problem5 = problem 5 part1 part2
