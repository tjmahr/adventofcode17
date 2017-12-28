# adventofcode17

These are my solutions to [Advent of Code 2017](http://adventofcode.com/2017), a
series of 25 programming puzzles.

The `R` folder contains the R functions I wrote for each day. I used some light
test-driven development for the puzzles. That is, each puzzle description
provides some example inputs and outputs. Before tackling the main test input, I
write a unit-test in `tests` that confirms that my solution can correctly
reproducet those examples. The `inst` directory contains the code to handle the
main test input for each day.

I limited the amount of package dependencies used for these puzzles to maximize
future compatibility and to make sure that it is mostly _my code_ that is
solving the problems. So far I have allowed myself to use:

* base, stats, utils, tools, and so on: the base R packages
* magrittr: for the pipe `%>%` syntax
* rlang: for language and code evaluation
* stringr: for regular-expression-related functions

I've put my R source code under a GPL-3 license. It should not apply to the
puzzle descriptions in the code comments at the top of each file. I did not
write those descriptions.

## Coding approaches

Here are the programming tasks and techniques I used for the various
puzzles.

- 01a/b _Compute different kinds of sums for elements in a circular vector:_ 
  Book-keeping (reordering indices).
  
- 02a/b _Compute different kinds of checksums on integer sequences:_ 
  Functional programming.
  
- 03a _If positive integers are in a spiral, find taxicab distance of 
  _n_ from 1:_ Math.
- 03b _In a different kind of spiral, find first number after _n_:_ 
  Enumerate values.
  
- 04a/b _Check if strings meet certain rules:_ Functional programming.

- 05a/b _Given a list of jump instructions, compute number of jumps required to 
  exit:_ Objects.
  
- 06a/b _Detect when an iterative process begins to loop forever:_ 
  Book-keeping (history of values).
  
- 07a/b _Construct a tree from a list of edges and check if it's balanced:_ 
  Recursion, tree-traversal, functional programming.
  
- 08a/b _Simulate a register machine:_ Custom evaluation (parse and evaluate 
  instructions as R code in a special environment).
  
- 09a/b _Count nesting depth of bracketed text that contains escape characters 
  and ignore sequences:_ Consume character stream.
  
- 10a/b _"Twist" a circular vector and hash strings with repeated twisting:_ 
  Book-keeping (reordering elements, converting strings to bits).

- 11a/b _Find shortest path between two cells in a hexagonal grid:_ Math.

- 12a/b _Find number of nodes in a connected graph and number of connected 
  subgroups in a graph:_ Breadth first search.
  
- 13a/b _Collision detection and avoidance through a_ Frogger<em>-style obstacle 
  course:</em> Math.

- 14a/b _Write a flood-fill algorithm:_ Breadth first search.

- 15a/b _Compare values from two generators:_ Enumerate values, 
  functional programming.

- 16a _Apply a series of permutation rules to a vector:_ Custom evaluation.
- 16b _Repeatedly apply the rules one billion times_: Book-keeping (repeat 
  until a duplicate permutation found).

- 17a _Repeatedly insert values and jump position in a circular vector:_ 
  Book-keeping (arithmetic on indices).
- 17b _Find value in a given position after 50 million cycles:_ Math.

- 18a/b _Simulate register machines that pass messages between each other:_ 
  Custom evaluation, objects.

- 19a/b _Make a cursor follow a path:_ Generate and consume character stream.

- 20a/b _Find slowest moving particle in a swarm and number of particles that 
      will eventually collide:_ Math.

- 21a/b _Find and replace patterns in a grid (like a fractal):_ Book-keeping 
  (keeping track of indices of subgrids).

- 22a/b _Move a cursor around an infinite grid and change symbols:_ Objects.

- 23a _Simulate register machine:_ Custom evaluation, objects.
- 23b _Compute value in an inefficient register machine:_ Analyzing code.

- 24a/b _Find largest/longest combinations of units:_ Recursion, 
  functional programming.
  
- 25a _Simulate a Turing Machine:_ Objects.
