module Day14 (solve1, part1, part2) where

import           ReadInput        (inputPath, readLines)

import           Control.Lens
import           Data.Either      (fromRight)
import qualified Data.Map         as M
import           Debug.Trace
import           Text.Parsec      hiding (Line)
import           Text.Parsec.Char

-- Parsing inputs

type Info = (String, Int)
type Line = ([Info], Info)

parseNum :: Parsec String a String
parseNum = many digit
parseIdent = many letter

parseInfo :: Parsec String a Info
parseInfo = do
    n <- read <$> parseNum
    spaces
    ident <- parseIdent
    return (ident, n)

parseSep = char ',' >> spaces

leftSide :: Parsec String a [Info]
leftSide = parseInfo `sepBy` parseSep

parseLine :: Parsec String a Line
parseLine = do
    l <- leftSide
    spaces >> string "=>" >> spaces -- arrow
    r <- parseInfo
    return (l, r)

getLines :: FilePath -> IO [Line]
getLines path = map (fromRight undefined . parse parseLine "undefined") <$> readLines path

-- model

type HelperMap = M.Map String (Int, [Info])

mkHelperMap :: [Line] -> HelperMap
mkHelperMap = foldl (\m (ls, (rname, batch)) -> M.insert rname (batch, ls) m) M.empty

-- a A -> b B, I want n B - how many B do I have to make?
minBatch :: Int -> Int -> Int
minBatch b n
    | n `mod` b == 0 = n
    | otherwise = (n `div` b + 1) * b

-- a A -> b B, I want n B - how many A do I have to make?
minCreated :: Int -> Int -> Int -> Int
minCreated _ _ 0 = 0
minCreated a b n = minBatch b n * a `div` b

fromOre :: HelperMap -> String -> Bool
fromOre hm name = let (_, ins) = hm M.! name in "ORE" `elem` map fst ins

step' :: HelperMap -> Info -> M.Map String Int
step' hm (name, n) = M.fromList $ map (_2 %~ transformA) ins
    where
        transformA a = minCreated a b actualN
        actualN = minBatch b n
        (b, ins) = hm M.! name

step :: HelperMap -> M.Map String Int -> M.Map String Int
step hm m
    | M.null m = M.empty
    | otherwise = M.unionsWith (+) [done, rest, step hm combined]
    where
        combined = M.unionsWith (+) outs
        outs = map (step' hm) $ M.toList rest
        (done, rest) = M.partitionWithKey (\name _ -> fromOre hm name) m

paths :: HelperMap -> M.Map String Int
paths hm = paths' hm "A"
    where
        paths' :: HelperMap -> String -> M.Map String Int
        paths' hm name
            | fromOre hm name = M.singleton "ORE" 1
            | otherwise = M.unionsWith (+) $ ones : map (paths' hm) names
                where
                    ones = M.fromList [(n, 1) | n <- names]
                    names = map fst infos
                    (_, infos) = hm M.! name

step1 :: HelperMap -> M.Map String Int -> M.Map String Int -> M.Map String Int
step1 hm cm m
    | M.null m = M.empty
    | otherwise = M.unionsWith (+) [done, rest, step1 hm cm combined]
    where
        combined = M.unionsWith (+) outs
        outs = map (step' hm) $ M.toList rest
        (done, rest) = M.partitionWithKey (\name _ -> fromOre hm name) m

getOre :: HelperMap -> Info -> Int
getOre hm (name, n) = case hm M.! name of
    (b, [("ORE", a)]) -> minCreated a b n
    _                 -> 0

step2 :: HelperMap -> M.Map String Int -> Int
step2 hm = M.foldlWithKey (\acc k v -> acc + getOre hm (k, v)) 0

solve1 :: FilePath -> IO Int
solve1 path = do
    hm <- mkHelperMap <$> getLines path
    let m = step hm $ M.fromList [("FUEL", 1)]
    return $ step2 hm m

-- TODO: solve
part1, part2 :: IO Int
part1 = return 0
part2 = return 0
