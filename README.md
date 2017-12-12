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
