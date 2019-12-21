module DaysSpec (spec) where

import Day1
import Day10
import Day11
import Day12
import Day13
import Day14
import Day16
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9

import DaysTH     (dayParts')

import Test.Hspec (Spec, describe, it, parallel, shouldBe)

testDay :: (Show a, Eq a, Show b, Eq b) => (Int, IO a, IO b) -> (a, b) -> Spec
testDay (day, in1, in2) (exp1, exp2) = describe ("Day " ++ show day) $ do
    it "Solves part 1" $ in1 >>= (`shouldBe` exp1)
    it "Solves part 2" $ in2 >>= (`shouldBe` exp2)

spec :: Spec
spec = parallel $ do
    testDay $(dayParts' 1) (3389778, 5081802)
    testDay $(dayParts' 2) (11590668, 2254)
    testDay $(dayParts' 3) (2193, 63526)
    testDay $(dayParts' 4) (1873, 1264)
    testDay $(dayParts' 5) (12896948, 7704130)
    testDay $(dayParts' 6) (247089, 442)
    testDay $(dayParts' 7) (77500, 22476942)
    testDay $(dayParts' 8) (2413, "BCPZB")
    testDay $(dayParts' 9) (4234906522, 60962)
    testDay $(dayParts' 10) (319, 517)
    testDay $(dayParts' 11) (1883, "APUGURFH")
    testDay $(dayParts' 12) (6735, 326489627728984)
    testDay $(dayParts' 13) (273, 13140)
    testDay $(dayParts' 14) (654909, 2876992)
    testDay $(dayParts' 16) (29795507, 89568529)
