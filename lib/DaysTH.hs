{-# LANGUAGE QuasiQuotes #-}

module DaysTH (runAllDays, inputPath, dayParts') where

import Data.Char                  (isAlpha)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)
import System.Directory           (listDirectory)

-- INPUT PATH RESOLUTION

-- Get current module name
moduleName :: Q Exp
moduleName = lift =<< loc_module <$> location

-- Get input path based on current module
inputPath :: Q Exp
inputPath = [| "inputs/input" ++ dropWhile isAlpha $(moduleName) ++ ".txt" |]

-- RUNNING ALL PUZZLES

-- Get both parts of a given puzzle
dayParts :: Int -> Q Exp
dayParts n = [| ($(varE (mkName part1)), $(varE (mkName part2))) |]
    where
        part1 = moduleName ++ ".part1"
        part2 = moduleName ++ ".part2"
        moduleName = "Day" ++ show n

-- Get an index and both parts of a given puzzle, used in unit tests
dayParts' :: Int -> Q Exp
dayParts' n = [| let (p1, p2) = $(dayParts n) in (n, p1, p2) |]

-- Run the module for a given day
runDay :: Int -> Q Exp
runDay n = [| let (part1, part2) = $(dayParts n) in do
    putStrLn $ "Day " ++ show n
    putStr "\tPart 1: "
    print =<< part1
    putStr "\tPart 2: "
    print =<< part2 |]

-- Run all Day modules up to a given day
runDays :: Int -> Q Exp
runDays 0 = [| putStrLn "Solutions for Advent of Code 2019!" |]
runDays n = [| $(runDays (n - 1)) >> $(runDay n) |]

-- Run all Day modules
runAllDays :: Q Exp
runAllDays = runDays =<< runIO (length <$> listDirectory "days")
