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
readLines path = lines <$> readFile path

readInts :: FilePath -> IO [Int]
readInts path = map read <$> readLines path

readCSV :: FilePath -> IO [[String]]
readCSV path = map splitCSV <$> readLines path
    where
        splitCSV :: String -> [String]
        splitCSV = map unpack . splitOn "," . pack

readProgram :: FilePath -> IO [Int]
readProgram path = map read . head <$> readCSV path
