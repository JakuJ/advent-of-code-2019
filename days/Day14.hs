module Day14 (part1, part2) where

import           ReadInput           (inputPath, readLines)

import           Control.Lens
import           Control.Monad       (filterM)
import           Control.Monad.State
import           Data.Either         (fromRight)
import           Data.List           (partition)
import qualified Data.Map            as M
import           Data.Maybe          (listToMaybe)
import           Text.Parsec         hiding (Line, State)
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

-- Data model

type HelperMap = M.Map String (Int, [Info])
type CountMap = M.Map String Int
type Stash = M.Map String (Int, Int)

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

-- is this substance made from ORE?
fromOre :: HelperMap -> String -> Bool
fromOre hm name = let (_, ins) = hm M.! name in "ORE" `elem` map fst ins

-- descending the dependency graph, how many times will we arrive at a given node?
paths :: HelperMap -> CountMap
paths hm = M.insert "FUEL" 1 $ paths' hm "FUEL"
    where
        paths' :: HelperMap -> String -> CountMap
        paths' hm name
            | fromOre hm name = M.singleton "ORE" 1
            | otherwise = M.unionsWith (+) $ ones : map (paths' hm) names
                where
                    ones = M.fromList [(n, 1) | n <- names]
                    names = map fst infos
                    (_, infos) = hm M.! name

data Factory = Factory {_helperMap :: HelperMap, _countMap :: CountMap, _stash :: Stash}
    deriving Show

makeLenses ''Factory

-- can I simplify this substance?
canSimplify :: (String, (a, Int)) -> State Factory Bool
canSimplify (name, (_, mult)) = do
    cm <- use countMap
    return $ cm M.! name == mult && name /= "ORE"

-- simplify a substance
simplify :: State Factory Stash
simplify = do
    st <- use stash
    -- find any substance in the stash I can simplify
    substance <- fmap listToMaybe . filterM canSimplify $ M.assocs st
    case substance of
        Nothing -> use stash -- we can't simplify further
        Just (name, (n, mult)) -> do
            (b, ins) <- (M.! name) <$> use helperMap
            let transformA a = (minCreated a b n, mult)
            let produced = M.fromList $ map (_2 %~ transformA) ins
            stash %= M.delete name
            stash %= M.unionWith (\(a, b) (c, d) -> (a + c, b + d)) produced
            simplify

-- how much ORE do I need for this much fuel?
solve :: HelperMap -> Int -> Int
solve hm k = fst $ val M.! "ORE"
    where
        factory = Factory hm (paths hm) $ M.singleton "FUEL" (k, 1)
        val = evalState simplify factory

part1 :: IO Int
part1 = do
    hm <- mkHelperMap <$> getLines $(inputPath)
    return $ solve hm 1

-- PART 2

binSearch :: HelperMap -> Int -> Int
binSearch hm k = search hm 0 k
    where
        search hm a b
            | canLeft && not canRight = n
            | canLeft = search hm n b
            | otherwise = search hm a n
            where
                n = (b + a) `div` 2
                canLeft = solve hm n <= k
                canRight = solve hm (n + 1) <= k

part2 :: IO Int
part2 = do
    hm <- mkHelperMap <$> getLines $(inputPath)
    return $ binSearch hm 1000000000000
