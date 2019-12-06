module Main where

import Problem1
import Problem2
import Problem3

main :: IO ()
main = sequence_ [problem1, problem2, problem3]
