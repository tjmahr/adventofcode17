library(adventofcode17)
day08_test <- readLines("./inst/input08.txt")
results <- run_register_instructions(day08_test)

stopifnot(results$max_final == aoc17_solutions$day08a)
stopifnot(results$max_ever == aoc17_solutions$day08b)
