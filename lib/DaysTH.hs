{-# LANGUAGE QuasiQuotes #-}

module DaysTH (runAllDays, dayParts') where

import Data.List                 (sort)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Directory          (listDirectory)

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
runDays :: [Int] -> Q Exp
runDays [] = [| putStrLn "Solutions for Advent of Code 2019!" |]
runDays (x:xs) = [| $(runDay x) >> $(runDays xs) |]

-- Run all Day modules
runAllDays :: Q Exp
runAllDays = runDays =<< runIO (sort . map (read . drop 3 . takeWhile (/= '.')) <$> listDirectory "days")
