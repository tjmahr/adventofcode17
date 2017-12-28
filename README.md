# adventofcode17

These are my solutions to [Advent of Code 2017](http://adventofcode.com/2017), a
series of 25 programming puzzles.

The `R` folder contains the R functions I wrote for each day. I used some light
test-driven development for the puzzles. That is, each puzzle description
provides some example inputs and outputs. Before tackling the main test input, I
write a unit-test in `tests` that confirms that my solution can correctly
reproducet those examples. The `inst` directory contains the code to handle the
main test input for each day.

I limited the amount of package dependencies used for these puzzles so that I do
as much of the heavy lifting as possible. So far I have allowed myself to use:

* base, stats, utils, tools, and so on: the base R packages
* magrittr: for the pipe `%>%` syntax
* rlang: for language and code evaluation
* stringr: for regular-expression-related functions

I've put my R source code under a GPL-3 license. It should not apply to the
puzzle descriptions in the code comments at the top of each file. I did not
write those descriptions, obviously.

Coding approaches

- 01a : .
- 01b : .
- 02a : .
- 02b : .
- 03a : .
- 03b : .
- 04a : .
- 04b : .
- 05a : .
- 05b : .
- 06a : .
- 06b : .
- 07a : .
- 07b : .
- 08a : .
- 08b : .
- 09a : .
- 09b : .
- 10a : .
- 10b : .
- 11a : .
- 11b : .
- 12a : .
- 12b : .
- 13a : .
- 13b : .
- 14a : .
- 14b : .
- 15a : .
- 15b : .
- 16a : .
- 16b : .
- 17a : .
- 17b : .
- 18a/b Simulate register machine: Nonstandard evaluation, object 
  oriented programming.
- 19a : .
- 19b : .
- 20a : .
- 20b : .
- 21a : .
- 21b : .
- 22a/b Move a cursor around a grid and change symbols: Object oriented
  programming.
- 23a Simulate register machine: Nonstandard evaluation, object 
  oriented programming.
- 23b Compute value in an _inefficient_ register machine: Analyzing code.
- 24a/b Find largest/longest combinations of units: Recursion, 
  functional programming.
- 25a Create a Turing Machine: Object oriented programming.
- 25b : .
