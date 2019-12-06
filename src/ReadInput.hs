{-# LANGUAGE OverloadedStrings #-}

module ReadInput (
    readLines,
    readIntegers,
    readCSV
) where

import Data.Text.Lazy (Text, pack, unpack, splitOn)

-- Helper functions

toInputPath :: FilePath -> FilePath
toInputPath = ("inputs/" ++ )

-- Exported reader functions

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile (toInputPath path)

readIntegers :: FilePath -> IO [Integer]
readIntegers path = map read <$> readLines path

readCSV :: FilePath -> IO [[String]]
readCSV path = map splitCSV <$> readLines path
    where
        splitCSV :: String -> [String]
        splitCSV = map unpack . splitOn "," . pack