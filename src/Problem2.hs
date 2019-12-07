module Problem2 (problem2) where

import IntCode                  (Computer, getAt, makeComputer, makeMemory,
                                 runComputer, setAt)
import Problem                  (problem)
import ReadInput                (readProgram)

import Control.Lens             ((^.), _3)
import Control.Monad.State.Lazy (evalState, execState)

-- PART 1

initialize :: Int -> Int -> Computer -> Computer
initialize v1 v2 = execState (setAt 1 v1 >> setAt 2 v2)

initComputer :: IO Computer
initComputer = do
    memory <- makeMemory <$> readProgram "input2.txt"
    return $ makeComputer memory []

getFirst :: Computer -> Int
getFirst = evalState (getAt 0)

process :: Int -> Int -> Computer -> Int
process v1 v2 = getFirst . runComputer . initialize v1 v2

part1 :: IO Int
part1 = process 12 2 <$> initComputer

-- PART 2

init3 :: Int -> Int -> Computer -> (Int, Int, Int)
init3 noun verb cmp = (noun, verb, process noun verb cmp)

tryAll :: Computer -> [(Int, Int, Int)]
tryAll comp = do
    noun <- [0 .. 99]
    verb <- [0 .. 99]
    return $ init3 noun verb comp

part2 :: IO Int
part2 = do
    allPossible <- tryAll <$> initComputer
    let (noun, verb, _) = head $ filter ((== 19690720) . (^. _3)) allPossible
    return $ 100 * noun + verb

-- EXPORTED SOLUTION

problem2 :: IO ()
problem2 = problem 2 part1 part2
