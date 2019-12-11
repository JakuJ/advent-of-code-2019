{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ReadInput (
    readLines,
    readInts,
    readCSV,
    readProgram,
    inputPath
) where

import Data.Char                  (isAlpha)
import Data.Text.Lazy             (Text, pack, splitOn, unpack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)

-- Input path resolution

-- Get current module name
moduleName :: Q Exp
moduleName = lift =<< loc_module <$> location

-- Get input path based on current module
inputPath :: Q Exp
inputPath = [| "inputs/input" ++ dropWhile isAlpha $(moduleName) ++ ".txt" |]

-- Input file parsing

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readInts :: FilePath -> IO [Int]
readInts = fmap (map read) . readLines

readCSV :: FilePath -> IO [[String]]
readCSV = fmap (map splitCSV) . readLines
    where
        splitCSV :: String -> [String]
        splitCSV = map unpack . splitOn "," . pack

readProgram :: FilePath -> IO [Integer]
readProgram = fmap (map read . head) . readCSV
