{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ReadInput (
    readLines,
    readInts,
    readCSV,
    readProgram,
    inputPath,
    withRawStdin,
    withRawStdout
) where

import Control.Exception          (bracket)
import Data.Char                  (isAlpha)
import Data.Text.Lazy             (Text, pack, splitOn, unpack)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)
import System.IO

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

-- User interactivity

withRawStdin :: IO a -> IO a
withRawStdin = bracket uncook restore . const
    where
        uncook = do
            oldBuffering <- hGetBuffering stdin
            oldEcho <- hGetEcho stdin
            hSetBuffering stdin NoBuffering
            hSetEcho stdin False
            return (oldBuffering, oldEcho)
        restore (oldBuffering, oldEcho) = do
            hSetBuffering stdin oldBuffering
            hSetEcho stdin oldEcho

withRawStdout :: IO a -> IO a
withRawStdout = bracket uncook restore . const
    where
        uncook = do
            oldBuffering <- hGetBuffering stdout
            hSetBuffering stdout NoBuffering
            return oldBuffering
        restore = hSetBuffering stdout
