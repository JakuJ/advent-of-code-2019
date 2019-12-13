module Main where

import Day1
import Day10
import Day11
import Day12
import Day13
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9

import DaysTH    (runAllDays)
import ReadInput (withRawStdin)

menu :: IO Char
menu = withRawStdin $ do
    putStrLn "Advent of Code 2019!"
    putStrLn "Choose your adventure:"
    putStrLn " 1 - Run all puzzles"
    putStrLn " 2 - Play Breakout on the arcade cabinet"
    putStrLn " 3 - Watch a bot beat the game"
    putStr " ...?: "
    getChar

main :: IO ()
main = do
    option <- menu
    case option of
        '1' -> putStrLn "" >> $(runAllDays)
        '2' -> Day13.playGame
        '3' -> Day13.watchBot
        _   -> return ()

