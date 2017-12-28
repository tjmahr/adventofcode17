library(adventofcode17)
xs <- c(4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3)

cycles <- analyze_reallocations(xs)

stopifnot(cycles$cycles_until_repeat == aoc17_solutions$day06a)
stopifnot(cycles$loop_length == aoc17_solutions$day06b)
