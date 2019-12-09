{-# LANGUAGE QuasiQuotes #-}

module DaysTH (runAllDays, inputPath, dayParts) where

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
inputPath = [| "inputs/input" ++ last $(moduleName) : ".txt" |]

-- RUNNING ALL PUZZLES

-- Get an index and both parts of a given day
dayParts :: Int -> Q Exp
dayParts n = [| (n, $(varE (mkName part1)), $(varE (mkName part2))) |]
    where
        moduleName = "Day" ++ show n
        part1 = moduleName ++ ".part1"
        part2 = moduleName ++ ".part2"

-- Run the module for a given day
runDay :: Int -> Q Exp
runDay n = [| let (_, part1, part2) = $(dayParts n) in do
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
