{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module IntCode (
    Computer (..),
    execComputer,
    execMemory,
    execProgram,
    programToMemory,
    memoryToComputer,
    programToComputer,
    setAt,
    getAt,
    supplyInputs,
    inputs,
    outputs
) where

import Control.Lens             hiding (index)
import Control.Monad            (mapM_)
import Control.Monad.State.Lazy
import Data.Array               hiding (index)
import Data.Maybe               (listToMaybe)

-- DATA TYPES

data Mode = Positional | Immediate
    deriving (Show)

type Memory = Array Int Int

data Computer = Computer
                { _memory  :: Array Int Int
                , _index   :: Int
                , _inputs  :: [Int]
                , _outputs :: [Int]
                , _running :: Bool
                }
makeLenses ''Computer

instance Show Computer where
    show c = "Computer: " ++ show val ++ " at " ++ show ix ++ ", stdin: " ++ show ins ++ ", stdout: " ++ show outs
        where
            val = evalState current c
            ix = c ^. index
            ins = c ^. inputs
            outs = c ^. outputs


type Operation = [Mode] -> [Int] -> State Computer ()

-- OPCODE HELPERS

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

popInput :: State Computer (Maybe Int)
popInput = do
    val <- listToMaybe <$> use inputs
    case val of
        Nothing -> do
            running .= False
            return Nothing
        Just k -> do
            inputs %= tail
            return $ Just k

input :: Operation
input _ [arg] = mapM_ (setAt arg) =<< popInput

pushOutput :: Int -> State Computer ()
pushOutput = (outputs <>=) . pure

output :: Operation
output [mode] [arg] = pushOutput =<< getValue mode arg

jumpIf :: (Int -> Bool) -> Operation
jumpIf pred [m1, m2] [a1, a2] = do
    check <- pred <$> getValue m1 a1
    when check $ index <~ getValue m2 a2

jumpIfTrue, jumpIfFalse :: Operation
jumpIfTrue = jumpIf (/= 0)
jumpIfFalse = jumpIf (== 0)

comparison :: (Int -> Int -> Bool) -> Operation
comparison op [m1, m2, _] [a1, a2, addr] = do
    v1 <- getValue m1 a1
    v2 <- getValue m2 a2
    setAt addr $ if v1 `op` v2 then 1 else 0

compareLess, compareEqual :: Operation
compareLess = comparison (<)
compareEqual = comparison (==)

getInfo :: Int -> (Operation, Int)
getInfo op = case op `mod` 100 of
    99 -> (halt, 0)
    1  -> (add, 3)
    2  -> (mult, 3)
    3  -> (input, 1)
    4  -> (output, 1)
    5  -> (jumpIfTrue, 2)
    6  -> (jumpIfFalse, 2)
    7  -> (compareLess, 3)
    8  -> (compareEqual, 3)
    x  -> error $ "Invalid opcode: " ++ show x

-- EVALUATION

programToMemory :: [Int] -> Memory
programToMemory list = listArray (0, length list - 1) list

memoryToComputer :: Memory -> Computer
memoryToComputer mem = Computer mem 0 [] [] True

programToComputer :: [Int] -> Computer
programToComputer prog = memoryToComputer $ programToMemory prog

evaluate :: State Computer ()
evaluate = do
    -- save index
    oldIndex <- use index
    -- eval current operation
    op <- current
    let (operation, nArgs) = getInfo op
    let modes = getModes op nArgs
    args <- getN nArgs
    operation modes args
    -- move pointer
    stillRunning <- use running
    when stillRunning $ do
        newIndex <- use index
        when (oldIndex == newIndex) $ index += nArgs + 1

interpret :: State Computer ()
interpret = use running >>= flip when (evaluate >> interpret)

-- DIRECT INTERACTION

supplyInputs :: [Int] -> Computer -> Computer
supplyInputs ins = execState (inputs <>= ins)

execComputer :: Computer -> Computer
execComputer = execState (running .= True >> interpret)

execMemory :: Memory -> Computer
execMemory memory = execState interpret $ memoryToComputer memory

execProgram :: [Int] -> Computer
execProgram program = execComputer $ programToComputer program
