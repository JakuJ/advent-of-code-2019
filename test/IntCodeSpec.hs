module IntCodeSpec (spec) where

import Control.Lens  ((^.))
import Data.Foldable (toList)
import Day7          (findMaxThrust, findMaxThrust2)
import IntCode
import Test.Hspec


transforms :: (Program, Program) -> Expectation
transforms (program, state) = toList (execProgram program ^. memory) `shouldBe` state

execWithInputs :: Program -> [Integer] -> Computer
execWithInputs program ins = execComputer . supplyInputs ins $ programToComputer program

produces :: (Program, [Integer], [Integer]) -> Expectation
produces (program, ins, outs) = (execWithInputs program ins ^. outputs) `shouldBe` outs

spec :: Spec
spec = describe "IntCode Interpreter" $ do
    it "Ends up in a proper state" $ mapM_ transforms day2testCases
    it "Produces correct outputs (day 2)" $ mapM_ produces day5testCases
    it "Finds maximum amplified thrust" $ mapM_ (\(p, out) -> findMaxThrust p [0 .. 4] 0 `shouldBe` out) day7testCases1
    it "Finds maximum feedback loop thrust" $ mapM_ (\(p, out) -> findMaxThrust2 p [5 .. 9] [0] `shouldBe` out) day7testCases2
    it "Produces correct outputs (day 9)" $ mapM_ (\(p, out) -> produces (p, [], out)) day9testCases

day2testCases :: [(Program, Program)]
day2testCases = [
    ([1,0,0,0,99], [2,0,0,0,99]),
    ([2,3,0,3,99], [2,3,0,6,99]),
    ([2,4,4,5,99,0], [2,4,4,5,99,9801]),
    ([1,1,1,4,99,5,6,0,99], [30,1,1,4,2,5,6,0,99])]

day5Programs :: [(Program, Integer -> Bool)]
day5Programs = [
    ([3,9,8,9,10,9,4,9,99,-1,8], (== 8)),
    ([3,9,7,9,10,9,4,9,99,-1,8], (< 8)),
    ([3,3,1108,-1,8,3,4,3,99], (== 8)),
    ([3,3,1107,-1,8,3,4,3,99], (< 8)),
    ([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], (/=0)),
    ([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], (/=0))]

day5testCases :: [(Program, [Integer], [Integer])]
day5testCases = do
    (program, pred) <- day5Programs
    input <- [7, 9]
    let output = if pred input then [1] else [0]
    return (program, [input], output)

day7testCases1 :: [(Program, Integer)]
day7testCases1 = [
    ([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0], 43210),
    ([3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0], 54321),
    ([3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0], 65210)]

day7testCases2 :: [(Program, Integer)]
day7testCases2 = [
    ([3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5], 139629729),
    ([3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10], 18216)]

day9testCases :: [(Program, [Integer])]
day9testCases = [
    (quine, quine),
    ([1102,34915192,34915192,7,4,7,99,0], [34915192 ^ 2]),
    ([104,1125899906842624,99], [1125899906842624])]
        where
            quine = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
