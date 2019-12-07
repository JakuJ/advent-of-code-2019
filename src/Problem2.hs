{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Problem2 (problem2) where

import ReadInput (readCSV)

import Data.Array (Array, listArray, bounds, (!), (//))
import Control.Monad.State.Lazy (State, get, modify, evalState)
import Data.Bifunctor (bimap)
import Data.Tuple.Select (sel3)

readCommands :: IO [Int]
readCommands = map read . head <$> readCSV "input2.txt"

-- PART 1

type Memory = Array Int Int
type Computer = (Memory, Int)

makeMemory :: [Int] -> Memory
makeMemory list = listArray (0, length list - 1) list

loadProgram :: IO Memory
loadProgram = makeMemory <$> readCommands
    
execute :: State Computer Int
execute = do
    (memory, index) <- get
    let opCode = memory ! index
    
    if index < snd (bounds memory) && opCode /= 99 then do
        let arg1 = memory ! (index + 1)
        let arg2 = memory ! (index + 2)
        let address = memory ! (index + 3)

        let op = case opCode of 1 -> (+)
                                2 -> (*)
                                _ -> error "Invalid opCode"

        modify $ bimap (// [(address, op (memory ! arg1) (memory ! arg2))]) (+4)
        execute
    else return $ memory ! 0

initialize :: Int -> Int -> Memory -> Memory
initialize noun verb = (// [(1, noun), (2, verb)])

evaluate :: Int -> Int -> Memory -> Int
evaluate noun verb = evalState execute . (, 0) . initialize noun verb

part1 :: IO Int
part1 = evaluate 12 2 <$> loadProgram

-- PART 2

evaluateV :: Int -> Int -> Memory -> (Int, Int, Int)
evaluateV noun verb mem = (noun, verb, evaluate noun verb mem)

tryAll :: Memory -> [(Int, Int, Int)]
tryAll mem = do
    noun <- [0 .. 99]
    verb <- [0 .. 99]
    return $ evaluateV noun verb mem

part2 :: IO Int
part2 = do
    allPossible <- tryAll <$> loadProgram
    let (noun, verb, _) = head $ filter ((== 19690720) . sel3) allPossible
    return $ 100 * noun + verb

-- EXPORTED SOLUTION

problem2 :: IO ()
problem2 = do
    putStrLn "Problem 2:"
    putStr "\tPart 1: "
    print =<< part1
    putStr "\tPart 2: "
    print =<< part2