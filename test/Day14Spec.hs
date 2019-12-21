module Day14Spec (spec) where

import Day14         (solve1)

import Data.Foldable (forM_)
import Test.Hspec    (Expectation, Spec, describe, it, parallel, shouldReturn)

spec :: Spec
spec = parallel $ do
    describe "Day 14" $ forM_ answers $ \(path, ans) ->
        it "Passes test case" $ solve1 ("test/cases/" ++ path) `shouldReturn` ans

answers :: [(String, Int)]
answers = [("14_1.txt", 31), ("14_2.txt", 165), ("14_3.txt", 13312), ("14_4.txt", 180697)] -- ("14_5.txt", 2210736)
