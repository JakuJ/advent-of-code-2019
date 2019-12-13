{-# LANGUAGE LambdaCase #-}

module Day13 (part1, part2, playGame, watchBot) where

import IntCode
import ReadInput                      (inputPath, readProgram, withRawStdin,
                                       withRawStdout)

import Control.Concurrent
import Control.Exception              (bracket)
import Control.Lens
import Control.Monad                  (unless, when)
import Control.Monad.State.Lazy       (evalState, execState, get, liftIO)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT)
import Data.Foldable                  (forM_)
import Data.Maybe                     (fromMaybe, listToMaybe)
import System.Console.ANSI

type Triple a = (a, a, a)

loadSource :: IO Program
loadSource = readProgram $(inputPath)

triples :: [Integer] -> [Triple Int]
triples (a:b:c:xs) = (fromIntegral a, fromIntegral b, fromIntegral c) : triples xs
triples _          = []

part1 :: IO Int
part1 = do
    comp <- execProgram <$> loadSource
    let outs = filter (\x -> x ^. _3 == 2) . triples $ comp ^. outputs
    return . length $ outs

-- PART 2

-- GAME LOGIC

codeToChar :: Int -> Char
codeToChar = \case
    0 -> ' ' -- empty
    1 -> '#' -- wall
    2 -> 'X' -- block
    3 -> '–' -- paddle
    4 -> '*' -- ball
    _ -> error "Invalid tile code"

renderTile :: Triple Int -> IO ()
renderTile (x, y, c) = setCursorPosition y x >> putStr [codeToChar c]

renderScore :: Int -> IO ()
renderScore score = setCursorPosition 25 0 >> clearLine >> putStr "Score: " >> print score

process :: Triple Int -> IO ()
process t@(x, _, c) = case x of
    (-1) -> unless (c == 0) $ renderScore c
    _    -> renderTile t

render :: Computer -> IO ()
render comp = mapM_ process . triples $ comp ^. outputs

-- HUMAN-PLAYABLE GAME

waitFor :: Int -> IO (Maybe Char)
waitFor delay = do
    key <- newEmptyMVar
    withRawStdin . bracket (start key) cleanUp $ \_ -> takeMVar key
  where
    start key = do
        t1 <- forkIO $ do
            char <- getChar
            putMVar key (Just char)
        t2 <- forkIO $ do
            threadDelay delay
            putMVar key Nothing
        return (t1, t2)
    cleanUp (t1, t2) = do
        killThread t1
        killThread t2

keyToPaddle :: Maybe Char -> Integer
keyToPaddle = \case
    Just 'a' -> -1
    Just 'd' -> 1
    _ -> 0

gameLoop :: Computer -> IO ()
gameLoop comp = do
    x <- waitFor 1000000
    let keyCode = keyToPaddle x
    let newComp = execComputer . supplyInputs [keyCode] $ comp
    render newComp
    when (evalState current newComp /= 99) $ gameLoop (newComp & outputs .~ [])

-- A BOT, 'CAUSE THE GAME IS TOO HARD FOR US PUNY HUMANS

-- (paddle, ball, score)
botMove :: Bool -> Computer -> StateT (Triple Int) IO Int
botMove animate comp = do
    -- update paddle / ball positions if there are valid outputs
    forM_ (filterProject _1 _3 3 ts) (_1 .=)
    forM_ (filterProject _1 _3 4 ts) (_2 .=)
    -- update score information
    forM_ (filterProject _3 _1 (-1) ts) (_3 .=)
    -- return score or loop further
    if evalState current comp == 99
        then use _3 -- return score
        else do
        -- move the paddle towards the ball
        (paddle, ball, _) <- get
        let newComp = execComputer . supplyInputs [toInteger (ball - paddle)] $ comp & outputs .~ []
        -- render if animated
        when animate $ liftIO $ do
            threadDelay 10000
            render newComp
        botMove animate newComp
    where
        ts = triples $ comp ^. outputs
        filterProject proj sel n = fmap (^. proj) . listToMaybe . equalsBy sel n
        equalsBy sel n = filter (\x -> x ^. sel == n)

botLoop :: Bool -> Computer -> IO Int
botLoop animate comp = evalStateT (botMove animate comp) (0, 0, 0)

-- RETURN GAME SCORE

playOnScreen :: (Computer -> IO a) -> IO ()
playOnScreen loop = withRawStdout $ do
    clearScreen
    game <- execState (setAt 0 2) . programToComputer <$> loadSource
    hideCursor
    loop game
    showCursor
    putStrLn ""

playGame, watchBot :: IO ()
playGame = playOnScreen gameLoop
watchBot = playOnScreen (botLoop True)

part2 :: IO Int
part2 = do
    game <- execState (setAt 0 2) . programToComputer <$> loadSource
    botLoop False game
