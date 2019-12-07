module Problem4 (problem4) where

import Problem       (problem)

import Data.Function (on)
import Data.List     (group)

lower, upper :: Int
(lower, upper) = (136760, 595730)

twoAdjacent :: String -> Bool
twoAdjacent = any ((>= 2) . length) . group

nonDecreasing :: String -> Bool
nonDecreasing str = and $ zipWith (>=) (tail str) str

checkRules :: String -> Bool
checkRules = and . ([twoAdjacent, nonDecreasing] <*>) . pure

-- PART 1

bruteforce :: [String]
bruteforce = [show pass | pass <- [lower .. upper], checkRules (show pass)]

part1 :: Int
part1 = length bruteforce

-- PART 2

onlyTwoAdjacent :: String -> Bool
onlyTwoAdjacent = any ((== 2) . length) . group

part2 :: Int
part2 = length . filter onlyTwoAdjacent $ bruteforce

problem4 :: IO ()
problem4 = (problem 4 `on` return) part1 part2
