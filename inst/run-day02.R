library(adventofcode17)
x <- readLines("./inst/input02.txt")

p1 <- spreadsheet_checksum(x, max_min_pair,
                           function(x) max(x) - min(x))

p2 <- spreadsheet_checksum(x, evenly_divisible_pair,
                           function(x) max(x) / min(x))

stopifnot(p1 == aoc17_solutions$day02a)
stopifnot(p2 == aoc17_solutions$day02b)
