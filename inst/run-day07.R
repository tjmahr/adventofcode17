library(adventofcode17)
lines <- readLines("./inst/input07.txt")

program <- find_root_program(lines)
stopifnot(program$name == aoc17_solutions$day07a)

imbalance <- find_program_imbalance(lines)
stopifnot(imbalance == aoc17_solutions$day07b)
