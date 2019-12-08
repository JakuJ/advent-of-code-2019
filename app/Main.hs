{-# LANGUAGE TemplateHaskell #-}

module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7

import Days (runDays)

main :: IO ()
main = $(runDays 7)
