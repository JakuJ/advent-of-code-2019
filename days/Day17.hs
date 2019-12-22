module Day17 (part1, part2) where

import           IntCode     (execProgram, _outputs)
import           ReadInput   (inputPath, readProgram)

import           Data.Char   (chr)
import           Data.Maybe  (mapMaybe)
import           Data.Vector (Vector)
import qualified Data.Vector as V

cameraOutput :: IO [String]
cameraOutput = init . lines . map (chr . fromIntegral) . _outputs . execProgram <$> readProgram $(inputPath)

see :: IO ()
see = mapM_ putStrLn =<< cameraOutput

type Scaffold = Vector (Vector Char)

mkScaffold :: [String] -> Scaffold
mkScaffold = V.fromList . map V.fromList

getAt :: Scaffold -> (Int, Int) -> Char
getAt sc (x, y) = (sc V.! y) V.! x

alignment :: Scaffold -> (Int, Int) -> Maybe Int
alignment sc (x, y) =
    if all ((== '#') . getAt sc) hashes
        then Just $ x * y
        else Nothing
    where
        hashes = [(x, y), (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

part1 :: IO Int
part1 = do
    sc <- mkScaffold <$> cameraOutput
    let height = V.length sc
    let width = V.length $ V.head sc
    let aligns = mapMaybe (alignment sc) [(a, b) | a <- [1 .. width - 2], b <- [1 .. height - 2]]
    return $ sum aligns

-- PART 2

part2 = part1
