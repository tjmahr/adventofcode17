library(adventofcode17)
moves <- readLines("./inst/input16.txt")
programs <- "abcdefghijklmnop"

p1_reordered <- dance(programs, moves) %>%
  paste0(collapse = "")

p2_reordered <- dance_a_billion_times(programs, moves) %>%
  paste0(collapse = "")

stopifnot(p1_reordered == aoc17_solutions$day16a)
stopifnot(p2_reordered == aoc17_solutions$day16b)

