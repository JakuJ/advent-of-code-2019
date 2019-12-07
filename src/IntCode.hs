{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module IntCode (
    Computer (..),
    runComputer,
    runMemory,
    runProgram,
    makeComputer,
    makeMemory,
    setAt,
    getAt
) where

import Control.Lens             hiding (index)
import Control.Monad.State.Lazy
import Data.Array               hiding (index)
import Debug.Trace

-- DATA TYPES

type OpCode = Int
data Mode = Positional | Immediate
    deriving (Show)

type Memory = Array Int Int

data Computer = Computer {_memory :: Array Int Int, _index :: Int, _inputs :: [Int], _outputs :: [Int], _running :: Bool}
    deriving (Show)

makeLenses ''Computer

type Operation = [Mode] -> [Int] -> State Computer ()

-- OPCODE HELPERS

getOpCode :: Int -> OpCode
getOpCode = (`mod` 100)

getDigit :: Int -> String -> Char
getDigit ix = (!! ix) . (++ repeat '0') . reverse

getModes :: Int -> Int -> [Mode]
getModes op n = map (toMode . flip getDigit str) [2 .. 1 + n]
    where
        str = show op
        toMode = \case
            '0' -> Positional
            '1' -> Immediate

-- STATE HELPERS

getAt :: Int -> State Computer Int
getAt ix = (! ix) <$> use memory

getN :: Int -> State Computer [Int]
getN n = do
    c <- use index
    sequence [getAt (c + offset) | offset <- [1 .. n]]

current :: State Computer Int
current = getAt =<< use index

setAt :: Int -> Int -> State Computer ()
setAt ix val = memory %= (// [(ix, val)])

getValue :: Mode -> Int -> State Computer Int
getValue = \case
    Positional -> getAt
    Immediate -> return

-- OPERATIONS

halt :: Operation
halt _ _ = running .= False

binaryOp :: (Int -> Int -> Int) -> Operation
binaryOp op [m1, m2, _] [a1, a2, addr] = do
    v1 <- getValue m1 a1
    v2 <- getValue m2 a2
    setAt addr $ v1 `op` v2

add, mult :: Operation
add = binaryOp (+)
mult = binaryOp (*)

popInput :: State Computer Int
popInput = do
    val <- head <$> use inputs
    inputs %= tail
    return val

input :: Operation
input _ [arg] = do
    val <- popInput
    setAt arg val

pushOutput :: Int -> State Computer ()
pushOutput val = outputs %= (val:)

output :: Operation
output [mode] [arg] = pushOutput =<< getValue mode arg

getOperation :: OpCode -> Operation
getOperation = \case
    99 -> halt
    x -> listArray (1, 4) [add, mult, input, output] ! x

-- EVALUATION

makeMemory :: [Int] -> Memory
makeMemory list = listArray (0, length list - 1) list

makeComputer :: Memory -> [Int] -> Computer
makeComputer mem inputs = Computer mem 0 inputs [] True

numArgs :: OpCode -> Int
numArgs op
    | op == 99 = 0
    | op `elem` [1, 2] = 3
    | op `elem` [3, 4] = 1
    | otherwise = error "Unknown operation code"

evaluate :: State Computer ()
evaluate = do
    -- eval current operation
    op <- current
    let opCode = getOpCode op
    let operation = getOperation opCode
    let nArgs = numArgs opCode
    let modes = getModes op nArgs
    args <- getN nArgs
    operation modes args
    -- move pointer
    index += (nArgs + 1)

interpret :: State Computer ()
interpret = use running >>= flip when (evaluate >> interpret)

runComputer :: Computer -> Computer
runComputer = execState interpret

runMemory :: Memory -> [Int] -> Computer
runMemory memory inputs = execState interpret computer
    where
        computer = makeComputer memory inputs

runProgram :: [Int] -> [Int] -> Computer
runProgram program = runMemory $ makeMemory program

