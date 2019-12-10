# advent-of-code-2019

Problems from Advent of Code 2019 solved in Haskell.

## Installation

* Have the `stack` build system for Haskell installed
* Clone this repository
* Run `stack build` in the main folder to compile, or `stack run` to also run the app

## Structure

* Codes for each of the puzzles are in the `days` folder.
* The puzzle inputs are stored in `inputs` directory.
* Helper modules, including the **IntCode** interpreter are in `lib`.

The **IntCode** interpreter is covered by unit tests that check example programs from the puzzles' contents.
I also include unit tests for each solved puzzle, that assert whether a given module still outputs the correct value.
And yes, that means there exists a list of answers in `DaysSpec.hs`, but everyone gets different inputs for their puzzles, so that's not a problem.

## Some more advanced concepts

### Template Haskell

This program uses Template Haskell to avoid repetition of code in two ways.
* I use it to call each od the modules' exported functions (`day1`, `day2` ...) in the `Main` module.
* I resolve the input file path based on the module name, so that the input for module `Day6.hs` is known to be `inputs/input6.txt` at compile time and without using magic strings.

This means that input files in the `/inputs` are used in the compilation process. Removing or renaming any of them will cause a compile time error (YAY, metaprogramming!).

Template Haskell doesn't support import statement generation, so a wall of imports for each day in the `Main` module is unavoidable.

### Lens

The *lens* package is used in the **IntCode** interpreter (and sparsely throughout the code) to get, set and modify the state of the virtual machine. This, alongside the use of the `State` monad, allows for a concise and simple to understand implementation.