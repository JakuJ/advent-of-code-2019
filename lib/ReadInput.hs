{-# LANGUAGE OverloadedStrings #-}

module ReadInput (
    readLines,
    readInts,
    readCSV,
    readProgram,
    inputPath -- Re-export from DaysTH
) where

import Data.Text.Lazy (Text, pack, splitOn, unpack)
import DaysTH         (inputPath)

-- Exported reader functions

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

readInts :: FilePath -> IO [Int]
readInts = fmap (map read) . readLines

readCSV :: FilePath -> IO [[String]]
readCSV = fmap (map splitCSV) . readLines
    where
        splitCSV :: String -> [String]
        splitCSV = map unpack . splitOn "," . pack

readProgram :: FilePath -> IO [Int]
readProgram = fmap (map read . head) . readCSV
