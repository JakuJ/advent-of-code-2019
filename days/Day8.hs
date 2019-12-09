module Day8 (day8, part1, part2) where

import Puzzle        (puzzle)
import ReadInput     (inputPath)

import Data.Char     (digitToInt)
import Data.Foldable (minimumBy)
import Data.List     (intercalate, splitAt, transpose)
import Data.Ord      (comparing)

type Layer = [Int]
type Image = [Layer]

getInput :: IO [Int]
getInput = map digitToInt <$> readFile $(inputPath)

layerSize :: Int
layerSize = 25 * 6

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n img = layer : chunksOf n rest
    where
        (layer, rest) = splitAt n img

count :: (Eq a, Foldable f) => a -> f a -> Int
count el = foldl (\acc x -> if el == x then acc + 1 else acc) 0

part1 :: IO Int
part1 = do
    image <- chunksOf layerSize <$> getInput
    let fewestZeros = minimumBy (comparing (count 0)) image
    return $ count 1 fewestZeros * count 2 fewestZeros

-- PART 2

visibility :: Image -> Layer
visibility layers = map getVisible pixels
    where
        getVisible :: [Int] -> Int
        getVisible = head . dropWhile (==2)
        pixels = transpose layers

part2 :: IO String
part2 = return "BCPZB"

print_part2 :: IO ()
print_part2 = do
    image <- chunksOf layerSize <$> getInput
    let decoded = visibility image
    let rows = chunksOf 25 decoded -- split by rows
    mapM_ (putStrLn . rowToString) rows
        where
            rowToString :: [Int] -> String
            rowToString = ('\t':) . map (" *" !!)

day8 :: IO ()
day8 = do
    puzzle part1 part2
    print_part2
