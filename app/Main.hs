module Main where

import Problem1
import Problem2
import Problem3
import Problem4

main :: IO ()
main = sequence_ [problem1, problem2, problem3, problem4]
