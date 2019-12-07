module Problem7 (problem7) where

import IntCode
import Problem   (problem)
import ReadInput (readProgram)

import Data.List (delete, maximum)

type Program = [Int]

runAmplifier :: Program -> [Int] -> Int -> [Int]
runAmplifier program inputs phase = _outputs $ runProgram program $ phase : inputs

amplifierChain :: Program -> [Int] -> Int -> Int
amplifierChain program phases input = head $ foldl (runAmplifier program) [input] phases

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations xs = do
    first <- xs
    rest <- permutations $ delete first xs
    return $ first : rest

tryAll :: Program -> [Int] -> Int -> [Int]
tryAll program phases input = do
    phases <- permutations phases
    return $ amplifierChain program phases input

part1 :: IO Int
part1 = do
    program <- readProgram "input7.txt"
    return . maximum $ tryAll program [0 .. 4] 0



problem7 :: IO ()
problem7 = problem 7 part1 (return 0)

