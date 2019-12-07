module Main where

import Problem1
import Problem2
import Problem3
import Problem4
import Problem5
import Problem6

main :: IO ()
main = sequence_ [problem1, problem2, problem3, problem4, problem5, problem6]
