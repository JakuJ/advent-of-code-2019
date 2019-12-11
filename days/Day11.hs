{-# LANGUAGE LambdaCase #-}
module Day11 (part1, part2) where

import           IntCode
import           ReadInput                  (inputPath, readProgram)

import           Control.Lens
import           Control.Monad              (forM_, unless)
import           Control.Monad.State.Strict (State, execState)
import           Data.List                  (maximum, minimum)
import qualified Data.Map.Strict            as M
import           Data.Tuple                 (swap)

loadComputer :: IO Computer
loadComputer = programToComputer <$> readProgram $(inputPath)

data Color = Black | White
    deriving (Show, Enum)

type Vector = (Int, Int)
data Robot = Robot {_brain :: Computer, _direction :: Vector, _position :: Vector, _hull :: M.Map Vector Color}
    deriving (Show)

makeLenses ''Robot

up, down, right, left :: Vector
up = (0, 1)
down = (0, -1)
left = (-1, 0)
right = (1, 0)

turn :: Integer -> Vector -> Vector
turn = \case
    0 -> (leftTurn M.!)
    1 -> (rightTurn M.!)
    where
        toLeft = [(up, left), (left, down), (down, right), (right, up)]
        leftTurn = M.fromList toLeft
        rightTurn = M.fromList $ map swap toLeft

vAdd :: Vector -> Vector -> Vector
vAdd (a, b) (c, d) = (a + c, b + d)

initRobot :: Color -> Computer -> Robot
initRobot clr cmp = Robot cmp up (0, 0) $ M.fromList [((0, 0), clr)]

currentColor :: State Robot Color
currentColor = do
    pos <- use position
    h <- use hull
    return $ M.findWithDefault Black pos h

move :: State Robot ()
move = do
    -- get color underneath
    cur <- currentColor
    -- pass it to the computer
    brain %= execComputer . supplyInputs [fromIntegral $ fromEnum cur]
    outs <- use $ brain . outputs
    brain . outputs .= []
    -- check if finished
    unless (null outs) $ do
        let [clr, dir] = outs
        -- paint current location
        pos <- use position
        hull %= M.insert pos (toEnum (fromInteger clr))
        -- update direction
        newDir <- turn dir <$> use direction
        direction .= newDir
        -- move
        position %= vAdd newDir
        -- iterate
        move

-- PART 1

part1 :: IO Int
part1 = do
    robot <- initRobot Black <$> loadComputer
    let state = execState move robot
    return . M.size $ state ^. hull

-- PART 2

findBounds :: [Vector] -> (Vector, Int, Int) -- upper left (console start), width, height
findBounds vs = ((leftB, upperB), rightB - leftB, upperB - lowerB)
    where
        xs = map fst vs
        ys = map snd vs
        upperB = maximum ys
        lowerB = minimum ys
        leftB = minimum xs
        rightB = maximum xs

-- run this to print the answer
printHull :: IO ()
printHull = do
    robot <- initRobot White <$> loadComputer
    let h = execState move robot ^. hull
    let ((sx, sy), width, height) = findBounds $ M.keys h
    -- iterative AF
    forM_ [0 .. height] $ \y -> do
        forM_ [0 .. width] $ \x -> do
            let (cx, cy) = (sx + x, sy - y) -- console coordinates
            putStr . toChar $ M.findWithDefault Black (cx, cy) h
        putStrLn ""
    where
        toChar :: Color -> String
        toChar = \case
            Black -> " "
            White -> "#"

part2 :: IO String
part2 = return "APUGURFH"
