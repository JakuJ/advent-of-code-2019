module Puzzle (puzzle) where

puzzle :: (Show a, Show b) => Int -> IO a -> IO b -> IO ()
puzzle number part1 part2 = do
    putStrLn $ "Problem " ++ show number ++ ":"
    putStr "\tPart 1: "
    print =<< part1
    putStr "\tPart 2: "
    print =<< part2
