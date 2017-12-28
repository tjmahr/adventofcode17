library(adventofcode17)
x <- readLines("./inst/input01.txt")
p1 <- sum_of_digits_matching_next(x)
p2 <- sum_of_digits_matching_halfway_around(x)

stopifnot(p1 == aoc17_solutions$day01a)
stopifnot(p2 == aoc17_solutions$day01b)
