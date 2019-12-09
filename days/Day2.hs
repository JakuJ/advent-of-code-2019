module Day2 (part1, part2) where

import IntCode                  (Computer, execComputer, getAt,
                                 programToComputer, setAt)
import ReadInput                (inputPath, readProgram)

import Control.Lens             ((^.), _3)
import Control.Monad.State.Lazy (evalState, execState)

initComputer :: IO Computer
initComputer = programToComputer <$> readProgram $(inputPath)

initialize :: Integer -> Integer -> Computer -> Computer
initialize v1 v2 = execState (setAt 1 v1 >> setAt 2 v2)

process :: Integer -> Integer -> Computer -> Integer
process v1 v2 = evalState (getAt 0) . execComputer . initialize v1 v2

-- PART 1

part1 :: IO Integer
part1 = process 12 2 <$> initComputer

-- PART 2

init3 :: Integer -> Integer -> Computer -> (Integer, Integer, Integer)
init3 noun verb cmp = (noun, verb, process noun verb cmp)

tryAll :: Computer -> [(Integer, Integer, Integer)]
tryAll comp = do
    noun <- [0 .. 99]
    verb <- [0 .. 99]
    return $ init3 noun verb comp

part2 :: IO Integer
part2 = do
    allPossible <- tryAll <$> initComputer
    let (noun, verb, _) = head $ filter ((== 19690720) . (^. _3)) allPossible
    return $ 100 * noun + verb
