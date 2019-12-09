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

import           Control.Lens             hiding (index)
import           Control.Monad            (mapM_)
import           Control.Monad.State.Lazy
import           Data.Maybe               (fromMaybe, listToMaybe)
import           Data.Sequence            (Seq, fromList, update)
import qualified Data.Sequence            as Seq (lookup, replicate)

-- DATA TYPES

data Mode = Positional | Immediate | Relative
    deriving (Show)

type Memory = Seq Integer

data Computer = Computer
                { _memory        :: Memory
                , _index         :: Int
                , _inputs        :: [Integer]
                , _outputs       :: [Integer]
                , _running       :: Bool
                , _relative_base :: Int
                }
makeLenses ''Computer

instance Show Computer where
    show c = "Computer: " ++ show val ++ " at " ++ show ix ++ ", stdin: " ++ show ins ++ ", stdout: " ++ show outs
        where
            val = evalState current c
            ix = c ^. index
            ins = c ^. inputs
            outs = c ^. outputs


type Operation = [Mode] -> [Integer] -> State Computer ()

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
            '2' -> Relative

-- MEMORY MANAGEMENT

-- O(1)
endOffset :: Int -> State Computer Int
endOffset ix = (+1) . (ix -) . length <$> use memory

-- O(log(min(i, n - 1)))
getAt :: Int -> State Computer Integer
getAt ix = fromMaybe 0 . Seq.lookup ix <$> use memory

-- O(log(min(i, n - 1))), where i = index
setAt :: Int -> Integer -> State Computer ()
setAt ix val = do
    offset <- endOffset ix -- O(1)
    if offset > 0
        then memory <>= Seq.replicate (offset - 1) 0 |> val -- O(log m) + O(log(min(n, m))) where m = i - n
        else memory %= update ix val -- O(log(min(i, n - 1)))

-- STATE HELPERS

getN :: Int -> State Computer [Integer]
getN n = do
    c <- use index
    sequence [getAt (c + offset) | offset <- [1 .. n]]

current :: State Computer Int
current = fromInteger <$> (getAt =<< use index)

getValue :: Mode -> Integer -> State Computer Integer
getValue = \case
    Positional -> getAt . fromInteger
    Immediate -> return
    Relative -> \n -> do
        b <- use relative_base
        getAt (fromInteger n + b)

-- OPERATIONS

halt :: Operation
halt _ _ = running .= False

binaryOp :: (Integer -> Integer -> Integer) -> Operation
binaryOp op [m1, m2, _] [a1, a2, addr] = do
    v1 <- getValue m1 a1
    v2 <- getValue m2 a2
    setAt (fromInteger addr) $ v1 `op` v2

add, mult :: Operation
add = binaryOp (+)
mult = binaryOp (*)

popInput :: State Computer (Maybe Integer)
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
input _ [arg] = mapM_ (setAt (fromInteger arg)) =<< popInput

pushOutput :: Integer -> State Computer ()
pushOutput = (outputs <>=) . pure

output :: Operation
output [mode] [arg] = pushOutput =<< getValue mode arg

jumpIf :: (Integer -> Bool) -> Operation
jumpIf pred [m1, m2] [a1, a2] = do
    check <- pred <$> getValue m1 a1
    when check $ index <~ fromInteger <$> getValue m2 (fromInteger a2)

jumpIfTrue, jumpIfFalse :: Operation
jumpIfTrue = jumpIf (/= 0)
jumpIfFalse = jumpIf (== 0)

comparison :: (Integer -> Integer -> Bool) -> Operation
comparison op [m1, m2, _] [a1, a2, addr] = do
    v1 <- getValue m1 a1
    v2 <- getValue m2 a2
    setAt (fromInteger addr) $ if v1 `op` v2 then 1 else 0

compareLess, compareEqual :: Operation
compareLess = comparison (<)
compareEqual = comparison (==)

adjustRelativeBase :: Operation
adjustRelativeBase [mode] [arg] = (relative_base +=) =<< fromInteger <$> getValue mode (fromInteger arg)

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
    9  -> (adjustRelativeBase, 1)
    x  -> error $ "Invalid opcode: " ++ show x

-- EVALUATION

programToMemory :: [Integer] -> Memory
programToMemory = fromList

memoryToComputer :: Memory -> Computer
memoryToComputer mem = Computer mem 0 [] [] True 0

programToComputer :: [Integer] -> Computer
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

supplyInputs :: [Integer] -> Computer -> Computer
supplyInputs ins = execState (inputs <>= ins)

execComputer :: Computer -> Computer
execComputer = execState (running .= True >> interpret)

execMemory :: Memory -> Computer
execMemory memory = execState interpret $ memoryToComputer memory

execProgram :: [Integer] -> Computer
execProgram program = execComputer $ programToComputer program

