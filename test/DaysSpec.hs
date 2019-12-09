module DaysSpec (spec) where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import           DaysTH     (dayParts)

import           Test.Hspec

testDay :: (Show a, Eq a, Show b, Eq b) => (Int, IO a, IO b) -> (a, b) -> Spec
testDay (day, in1, in2) (exp1, exp2) = describe ("Day " ++ show day) $ do
    it "Solves part 1" $ in1 >>= (`shouldBe` exp1)
    it "Solves part 2" $ in2 >>= (`shouldBe` exp2)

spec :: Spec
spec = do
    testDay $(dayParts 1) (3389778, 5081802)
    testDay $(dayParts 2) (11590668, 2254)
    testDay $(dayParts 3) (2193, 63526)
    testDay $(dayParts 4) (1873, 1264)
    testDay $(dayParts 5) (12896948, 7704130)
    testDay $(dayParts 6) (247089, 442)
    testDay $(dayParts 7) (77500, 22476942)
    testDay $(dayParts 8) (2413, "BCPZB")
    testDay $(dayParts 9) (4234906522, 60962)
