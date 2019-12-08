{-# LANGUAGE QuasiQuotes #-}

module DaysTH (runDays, inputPath) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- Get current module name
moduleName :: Q Exp
moduleName = LitE . StringL . loc_module <$> location

-- Get input path based on current module
inputPath :: Q Exp
inputPath = [| "inputs/input" ++ last $(moduleName) : ".txt" |]

-- Run the module for a given day
runDay :: Int -> Q Exp
runDay n = [| putStrLn title >> $(varE (mkName function)) |]
    where
        title = "Day " ++ num ++ ":"
        function = "day" ++ num
        num = show n

runDays :: Int -> Q Exp
runDays 0 = [| return () |]
runDays n = [| $(runDays (n - 1)) >> $(runDay n) |]

