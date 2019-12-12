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

Optics are used extensively in the **IntCode** VM to get, set and modify different parts of the state. They are also handy when handling n-dimensional tuples, mostly when solving puzzles that require the use of 3D points or vectors.

### Pure parallelism

The computation required to solve the second part of the 12th day's puzzle is quite extensive, so a simple implementation of a parallel mapping is implemented using the `par` combinator from the `Control.Parallel` package. This speeds up things a bit on multicore machines.

I've ran `Day12.part2` 10 times using normal `map` and `parallelMap`. I estimated the 95% confidence intervals for mean execution time. Results are as follows: 

|Method       |Lower bound|Upper bound|
|-------------|-----------|-----------|
|`map`        |14.38167   |15.33233   |
|`parallelMap`|10.55612   |12.06788   |

Performing a Welch Two Sample t-test for difference in execution time resulted in a `[-4.38557, -2.70443]` CI. The conclusion is that the `parallelMap`'s performance improvement is statistically significant, meaning that the parallel execution is handled correctly.

### Strictness analysis

The second part of the puzzle for day 12 involves a really long iteration. It running for approx. 11 seconds (see above) wouldn't be surprising by itself, but I happened to run a profiler on the program. The results have shown the memory usage to be around ~1700 MB. Changing the definition for the `vec3` function at the beginning of the module from

```haskell
vec3 :: [Int] -> Vector
vec3 [a, b, c] = (a, b, c)
```

to

```haskell
vec3 :: [Int] -> Vector
vec3 [!a, !b, !c] = (a, b, c)
```

using the `BangPatterns` extension for strictness annotations reduced that to ~0 MB and improved the runtime from 11 seconds on average to about 1.7. The parallelism still helps – without it the program runs 2-3x slower (on my machine).