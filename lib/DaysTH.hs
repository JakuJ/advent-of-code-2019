{-# LANGUAGE QuasiQuotes #-}

module DaysTH (runAllDays, inputPath) where

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

-- Run all Da modules
runAllDays :: Q Exp
runAllDays = runDays =<< runIO (length <$> listDirectory "days")
