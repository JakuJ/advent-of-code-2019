module Day4 (part1, part2) where

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

part1 :: IO Int
part1 = return $ length bruteforce

-- PART 2

onlyTwoAdjacent :: String -> Bool
onlyTwoAdjacent = any ((== 2) . length) . group

part2 :: IO Int
part2 = return . length . filter onlyTwoAdjacent $ bruteforce
