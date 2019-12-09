module Day7 (day7, part1, part2, findMaxThrust, findMaxThrust2) where

import IntCode                  (Computer, Program, execComputer, inputs,
                                 outputs, programToComputer, supplyInputs)
import Puzzle                   (puzzle)
import ReadInput                (inputPath, readProgram)

import Control.Lens
import Control.Monad.State.Lazy
import Data.List                (delete, maximum, permutations)
import Data.Maybe               (catMaybes, listToMaybe)

type Inputs = [Integer]
type Phases = [Integer]
type Outputs = [Integer]

execAmplifier :: Program -> Inputs -> Integer -> Outputs
execAmplifier program ins phase = (^. outputs) . execComputer . supplyInputs (phase : ins) $ computer
    where
        computer = programToComputer program

execAmplifierChain :: Program -> Phases -> Integer -> Outputs
execAmplifierChain program phases input = foldl (execAmplifier program) [input] phases

findMaxThrust :: Program -> Phases -> Integer -> Integer
findMaxThrust program phases input = maximum $ do
    phases <- permutations phases
    return . head $ execAmplifierChain program phases input

part1 :: IO Integer
part1 = do
    program <- readProgram $(inputPath)
    return $ findMaxThrust program [0 .. 4] 0

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

findMaxThrust2 :: Program -> Phases -> Inputs -> Integer
findMaxThrust2 program allPhases ins = maximum . map head $ do
    phases <- permutations allPhases
    let comps = initLoop program phases
    return $ evalState (feedbackLoop ins) comps

part2 :: IO Integer
part2 = do
    program <- readProgram $(inputPath)
    return $ findMaxThrust2 program [5 .. 9] [0]

day7 :: IO ()
day7 = puzzle part1 part2

