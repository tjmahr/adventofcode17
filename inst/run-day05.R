library(adventofcode17)
x <- readLines("./inst/input05.txt")

p1 <- find_time_to_escape_trampolines(x)
p2 <- find_time_to_escape_twistolines(x)

stopifnot(p1 == aoc17_solutions$day05a)
stopifnot(p2 == aoc17_solutions$day05b)
