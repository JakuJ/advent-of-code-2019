module Day9 (part1, part2) where

import IntCode   (Computer (_outputs), execWithInputs)
import ReadInput (inputPath, readProgram)

getSource :: IO [Integer]
getSource = readProgram $(inputPath)

runBOOST :: Integer -> IO Integer
runBOOST x = head . _outputs . execWithInputs [x] <$> getSource

part1, part2 :: IO Integer
part1 = runBOOST 1
part2 = runBOOST 2
