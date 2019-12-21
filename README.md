# Advent of Code 2019

Problems from Advent of Code 2019 solved in Haskell, now with Breakout!

## Installation

* Have the `stack` build system for Haskell installed
* Clone this repository
* Run `stack build` in the main folder.

## Running

After running the app you will be presented with three options:
* Running all solutions
* Playing Breakout on an IntCode VM (day 13 puzzle)
* Watching a bot do it for you, because it's so frustrating to play

You can also run the unit test suite (`stack test`) to verify everything works correctly, especially the IntCode interpreter.

## Structure

* Codes for each of the puzzles are in the `days` folder.
* The puzzle inputs are stored in `inputs` directory.
* Helper modules, including the **IntCode** interpreter are in `lib`.

The **IntCode** interpreter is covered by unit tests that check example programs from the puzzles' contents.
I also include unit tests for each solved puzzle, that assert whether a given module still outputs the correct value.
And yes, that means there exists a list of answers in `DaysSpec.hs`, but everyone gets different inputs for their puzzles, so that's not a problem.

## Template Haskell

This program uses Template Haskell to avoid repetition of code in two ways.
* I use it to call each od the modules' exported functions (`day1`, `day2` ...) in the `Main` module.
* I resolve the input file path based on the module name, so that the input for module `Day6.hs` is known to be `inputs/input6.txt` at compile time and without using magic strings.

This means that input files in the `/inputs` are used in the compilation process. Removing or renaming any of them will cause a compile time error (YAY, metaprogramming!).

Template Haskell doesn't support import statement generation, so a wall of imports for each day in the `Main` module is unavoidable.
