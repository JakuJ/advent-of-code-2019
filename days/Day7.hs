module Day7 (day7) where

import IntCode                  (Computer, execComputer, inputs, outputs,
                                 programToComputer, supplyInputs)
import Puzzle                   (puzzle)
import ReadInput                (readProgram)

import Control.Lens
import Control.Monad.State.Lazy
import Data.List                (delete, maximum)
import Data.Maybe               (catMaybes, listToMaybe)

type Program = [Int]
type Inputs = [Int]
type Phases = [Int]
type Outputs = [Int]

execAmplifier :: Program -> Inputs -> Int -> Outputs
execAmplifier program ins phase = (^. outputs) . execComputer . supplyInputs (phase : ins) $ computer
    where
        computer = programToComputer program

execAmplifierChain :: Program -> Phases -> Int -> Outputs
execAmplifierChain program phases input = foldl (execAmplifier program) [input] phases

permutations :: Phases -> [Phases]
permutations [] = [[]]
permutations xs = do
    first <- xs
    rest <- permutations $ delete first xs
    return $ first : rest

tryAll :: Program -> Phases -> Int -> Outputs
tryAll program phases input = do
    phases <- permutations phases
    return . head $ execAmplifierChain program phases input

part1 :: IO Int
part1 = do
    program <- readProgram "input7.txt"
    return . maximum $ tryAll program [0 .. 4] 0

-- PART 2

initLoop :: Program -> Phases -> [Computer]
initLoop program = map (\ph -> supplyInputs [ph] initial)
    where
        initial = programToComputer program

processInputs :: Inputs -> Computer -> Computer
processInputs ins = execComputer . supplyInputs ins

loopPass :: [Computer] -> Inputs -> (Outputs, [Computer])
loopPass comps firstInputs = foldl walk (firstInputs, []) comps
    where
        walk (ins, comps) comp = (newComp ^. outputs, comps ++ [clearComp])
            where
                clearComp = newComp & outputs .~ []
                newComp = processInputs ins comp

feedbackLoop :: Inputs -> State [Computer] Outputs
feedbackLoop ins = do
    comps <- get
    let (outs, newComps) = loopPass comps ins
    if null outs then return ins else put newComps >> feedbackLoop outs

tryAll2 :: Program -> Phases -> Inputs -> [Outputs]
tryAll2 program allPhases ins = do
    phases <- permutations allPhases
    let comps = initLoop program phases
    return $ evalState (feedbackLoop ins) comps

part2 :: IO Int
part2 = do
    program <- readProgram "input7.txt"
    return . maximum . map head $ tryAll2 program [5 .. 9] [0]

day7 :: IO ()
day7 = puzzle 7 part1 part2

