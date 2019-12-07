module Problem5 (problem5) where

import IntCode
import Problem   (problem)
import ReadInput (readProgram)

runTEST :: Int -> IO [Int]
runTEST x = do
    program <- readProgram "input5.txt"
    return . _outputs $ runProgram program [x]

part1, part2 :: IO [Int]
part1 = runTEST 1
part2 = runTEST 5

problem5 :: IO ()
problem5 = problem 5 part1 part2
