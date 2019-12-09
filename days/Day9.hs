module Day9 (day9) where

import IntCode
import ReadInput (readProgram, inputPath)

getSource :: IO [Int]
getSource = readProgram $(inputPath)
    
day9 :: IO ()
day9 = putStrLn "Haha"