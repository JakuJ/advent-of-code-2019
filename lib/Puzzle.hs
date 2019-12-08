module Puzzle (puzzle) where

puzzle :: (Show a, Show b) => IO a -> IO b -> IO ()
puzzle part1 part2 = do
    putStr "\tPart 1: "
    print =<< part1
    putStr "\tPart 2: "
    print =<< part2
