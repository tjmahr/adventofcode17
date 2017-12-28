library(adventofcode17)
lines <- readLines("./inst/input09.txt")

score <- process_stream(lines)
stopifnot(score == aoc17_solutions$day09a)

garbage <- count_garbage(lines)
stopifnot(garbage == aoc17_solutions$day09b)
