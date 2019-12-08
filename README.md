# advent-of-code-2019

Problems from Advent of Code 2019 solved in Haskell.

### Installation

* Have the `stack` build system for Haskell installed
* Clone this repository
* Run `stack build` in the main folder to compile, or `stack run` to also run the app

### Structure

* Codes for each of the puzzles are in the `days` folder.
* The puzzle inputs are stored in `inputs` directory.
* Helper modules, including the **IntCode** interpreter are in `lib`.

### Some more advanced concepts

**Template Haskell**

This program uses Template Haskell to avoid repetition of code in two ways.
* I use it to call each od the modules' exported functions (`day1`, `day2` ...) in the `Main` module.
* I resolve the input file path based on the module name, so that the input for module `Day6.hs` is known to be `inputs/input6.txt` at compile time and without using magic strings.

Template Haskell doesn't support import statement generation, so a wall of imports for each day in the `Main` module is unavoidable.

**Lens**

The *lens* package is used in the **IntCode** interpreter too get, set and modify the state of the virtual machine. The interpreter uses the `State` monad to simplify stateful operations.