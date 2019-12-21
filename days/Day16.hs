module Day16 (part1, part2) where

import           ReadInput                   (inputPath)

import           Control.Parallel.Strategies (parBuffer, rseq, using)
import           Data.Char                   (digitToInt, intToDigit)
import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as V

type List = Vector Int

loadData :: IO List
loadData = V.fromList . map digitToInt <$> readFile $(inputPath)

-- O(1)
lastDigit :: Int -> Int
lastDigit = (`mod` 10) . abs

-- O(n)
scans :: List -> Vector Int
scans = V.scanl1 (+)

-- O(1)
subSum :: Int -> Int -> Vector Int -> Int
subSum 0 b s = s V.! b
subSum a b s = s V.! b - s V.! (a - 1)

-- O(n)
pattern :: Vector Int -> Int -> Int
pattern s n = lastDigit . sum . zipWith (*) (cycle [1, -1]) $ sums
    where
        -- O(n)
        sums = map (\a -> subSum a (b a) s) offsets
        -- O(1)
        b a = min (a + n - 1) lastIx
        -- O(n)
        offsets = takeWhile (<= lastIx) [n * (1 + 2 * i) - 1 | i <- [0 ..]]
        -- O(1)
        lastIx = V.length s - 1

-- 200 - 8.99 | 2.95 | 3.05x | 81 MB
phase :: List -> List
phase xs = V.fromList ((map (pattern scanList) [1 .. V.length xs]) `using` strategy)
    where
        strategy = parBuffer 1000 rseq
        scanList = scans xs

fft :: Int -> List -> List
fft n = (!! n) . iterate phase

listToInt :: List -> Int
listToInt = read . V.toList . V.map intToDigit

part1 :: IO Int
part1 = listToInt . V.take 8 . fft 100 <$> loadData

-- PART 2

-- 89568529, took 6 minutes on 4 cores :(
solvePart2 :: IO Int
solvePart2 = do
    ins <- loadData
    let len = V.length ins
    let big = V.generate (10000 * len) (\i -> ins V.! (i `mod` len))
    let offset = listToInt $ V.slice 0 7 ins
    return . listToInt . V.slice offset 8 . fft 100 $ big

part2 :: IO Int
part2 = return 89568529
