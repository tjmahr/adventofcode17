library(adventofcode17)
pipes <- readLines("./inst/input12.txt")

neighborhood <- search_pipes_from_zero(pipes)
groups <- count_pipe_groups(pipes)

stopifnot(length(neighborhood) == aoc17_solutions$day12a)
stopifnot(groups == aoc17_solutions$day12b)
