{-# LANGUAGE OverloadedStrings #-}

module ReadInput (
    readLines,
    readInts,
    readCSV,
    readProgram
) where

import Data.Text.Lazy (Text, pack, splitOn, unpack)

-- Helper functions

toInputPath :: FilePath -> FilePath
toInputPath = ("inputs/" ++ )

-- Exported reader functions

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile (toInputPath path)

readInts :: FilePath -> IO [Int]
readInts path = map read <$> readLines path

readCSV :: FilePath -> IO [[String]]
readCSV path = map splitCSV <$> readLines path
    where
        splitCSV :: String -> [String]
        splitCSV = map unpack . splitOn "," . pack

readProgram :: FilePath -> IO [Int]
readProgram path = map read . head <$> readCSV path
