{-# LANGUAGE QuasiQuotes #-}

module DaysTH (runAllDays, inputPath, dayParts) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)
import System.Directory           (listDirectory)

-- Get current module name
moduleName :: Q Exp
moduleName = lift =<< loc_module <$> location

-- Get input path based on current module
inputPath :: Q Exp
inputPath = [| "inputs/input" ++ last $(moduleName) : ".txt" |]

-- Run the module for a given day
runDay :: Int -> Q Exp
runDay n = [| putStrLn header >> $(varE (mkName funName)) |]
    where
        header = "Day " ++ show n ++ ":"
        funName = "day" ++ show n

-- Run all modules up to a given day
runDays :: Int -> Q Exp
runDays 0 = [| putStrLn "Solutions for Advent of Code 2019!" |]
runDays n = [| $(runDays (n - 1)) >> $(runDay n) |]

-- Run all Day modules
runAllDays :: Q Exp
runAllDays = runDays =<< runIO (length <$> listDirectory "days")

-- Get both parts of a given day
dayParts :: Int -> Q Exp
dayParts n = [| (n, $(varE (mkName part1)), $(varE (mkName part2))) |]
    where
        moduleName = "Day" ++ show n
        part1 = moduleName ++ ".part1"
        part2 = moduleName ++ ".part2"
