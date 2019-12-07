module Problem4 (problem4) where

import           Problem       (problem)

import           Control.Monad (guard)
import           Data.List     (group)

lower, upper :: Int
(lower, upper) = (136760, 595730)

twoAdjacent :: String -> Bool
twoAdjacent []       = False
twoAdjacent [x]      = False
twoAdjacent (x:y:xs) = x == y || twoAdjacent (y : xs)

nonDecreasing :: String -> Bool
nonDecreasing str = and $ zipWith (>=) (tail str) str

checkRules :: Int -> Bool
checkRules i = and [i >= lower, i <= upper, twoAdjacent str, nonDecreasing str]
    where
        str = show i

-- PART 1

bruteforce :: [Int]
bruteforce = do
    pass <- [lower .. upper]
    guard $ checkRules pass
    return pass

part1 :: IO Int
part1 = return $ length bruteforce

-- PART 2

onlyTwoAdjacent :: Int -> Bool
onlyTwoAdjacent = any ((== 2) . length) . group . show

part2 :: IO Int
part2 = return . length . filter onlyTwoAdjacent $ bruteforce

problem4 = problem 4 part1 part2
