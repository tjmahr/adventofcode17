
<!-- README.md is generated from README.Rmd. Please edit that file -->
adventofcode17
==============

These are my solutions to [Advent of Code 2017](http://adventofcode.com/2017), a series of 25 programming puzzles.

Package overview
----------------

The `/R` folder contains the R functions I wrote for each day. I used some light test-driven development for the puzzles. That is, each puzzle description provides some example inputs and outputs. Before tackling the main test input, I write a unit-test in `/tests` that confirms that my solution can correctly reproduce the examples. The `/inst` directory contains the code to handle the main test input for each day.

I limited the amount of package dependencies used for these puzzles to maximize future compatibility and to make sure that it is mostly *my code* that solves the problems. For example, if a puzzle requires answering questions about the data in a tree-like structure, it would be kind of cheating for me to find a library for building and traversing trees to tackle the problem. It's *advent of code*, not *advent of load*.

I have allowed myself to use:

-   base, stats, utils, tools, and so on: the base R packages
-   magrittr: for the pipe `%>%` syntax
-   rlang: for language and code evaluation
-   stringr: for regular-expression-related functions

I've put my R source code under a GPL-3 license. It should not apply to the puzzle descriptions in the code comments at the top of each file. I did not write those descriptions.

Coding approaches
-----------------

Here are the programming tasks and techniques I used for the various puzzles.

-   01a/b *Compute different kinds of sums for elements in a circular vector:* Book-keeping (reordering indices).

-   02a/b *Compute different kinds of checksums on integer sequences:* Functional programming.

-   03a *If positive integers are in a spiral, find taxicab distance of *n* from 1:* Math.
-   03b *In a different kind of spiral, find first number after *n*:* Enumerate values.

-   04a/b *Check if strings meet certain rules:* Functional programming.

-   05a/b *Given a list of jump instructions, compute number of jumps required to exit:* Objects.

-   06a/b *Detect when an iterative process begins to loop forever:* Book-keeping (history of values).

-   07a/b *Construct a tree from a list of edges and check if it's balanced:* Recursion, tree-traversal, functional programming.

-   08a/b *Simulate a register machine:* Custom evaluation (parse and evaluate instructions as R code in a special environment).

-   09a/b *Count nesting depth of bracketed text that contains escape characters and ignore sequences:* Consume character stream.

-   10a/b *"Twist" a circular vector and hash strings with repeated twisting:* Book-keeping (reordering elements, converting strings to bits).

-   11a/b *Find shortest path between two cells in a hexagonal grid:* Math.

-   12a/b *Find number of nodes in a connected graph and number of connected subgroups in a graph:* Breadth first search.

-   13a/b *Collision detection and avoidance through a* Frogger<em>-style obstacle course:</em> Math.

-   14a/b *Write a flood-fill algorithm:* Breadth first search.

-   15a/b *Compare values from two generators:* Enumerate values, functional programming.

-   16a *Apply a series of permutation rules to a vector:* Custom evaluation.
-   16b *Repeatedly apply the rules one billion times*: Book-keeping (repeat until a duplicate permutation found).

-   17a *Repeatedly insert values and jump position in a circular vector:* Book-keeping (arithmetic on indices).
-   17b *Find value in a given position after 50 million cycles:* Math.

-   18a/b *Simulate register machines that pass messages between each other:* Custom evaluation, objects.

-   19a/b *Make a cursor follow a path:* Generate and consume character stream.

-   20a/b *Find slowest moving particle in a swarm and number of particles that will eventually collide:* Math.

-   21a/b *Find and replace patterns in a grid (like a fractal):* Book-keeping (keeping track of indices of subgrids).

-   22a/b *Move a cursor around an infinite grid and change symbols:* Objects.

-   23a *Simulate register machine:* Custom evaluation, objects.
-   23b *Compute value in an inefficient register machine:* Analyzing code.

-   24a/b *Find largest/longest combinations of units:* Recursion, functional programming.

-   25a *Simulate a Turing Machine:* Objects.

By "book-keeping", I mean basic programming programming where I keep track of some changing state like a position in a vector.

By "math", I mean studying the problem and using math to find a shortcut that lets me skip some computations.

By "custom evaluation", I mean writing a parser to convert the problem input into R code and run that R code in a special environment. This technique changes the coding task into the task of running R code with some extra book-keeping. This approach was a new one for me, so I learned a great deal along the way and I could streamline my solutions for some puzzles if I had to redo them.

By "functional programming", I mean both map/filter/reduce operations as well as using first class functions. For example, on day 04, I have to count how many passphrases have a duplicated word in Part A and count how many passphrases contain anagrams in Part B. My solution is a function `count_valid_passphrases(passphrases, rule)` where `rule` is a function tailored for Part A or Part B.

By "objects", I mean object-oriented programming using closures. Something like:

``` r
counter <- function(start = 0) {
  initial <- force(start)
  num <- initial
  inc <- function() num <<- num + 1
  dec <- function() num <<- num - 1
  reset <- function() num <<- initial
  look <- function() num
  
  list(inc = inc, dec = dec, reset = reset, look = look)
}

robot <- counter(10)
robot$look()
#> [1] 10
robot$inc()
robot$inc()
robot$look()
#> [1] 12
robot$dec()
robot$dec()
robot$dec()
robot$dec()
robot$look()
#> [1] 8
robot$reset()
robot$look()
#> [1] 10
```
