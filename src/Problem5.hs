module Problem5 (problem5) where

import Problem   (problem)
import ReadInput (readProgram)
import IntCode

part1 :: IO [Int]
part1 = do
    program <- readProgram "input5.txt"
    return . _outputs $ runProgram program [1]

problem5 :: IO ()
problem5 = problem 5 part1 (return 0)
