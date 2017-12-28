library(adventofcode17)
results <- readLines("./inst/input11.txt") %>%
  strsplit(",") %>%
  unlist() %>%
  hexagon_distance()

stopifnot(results$n_x_steps + results$n_y_steps == aoc17_solutions$day11a)

results2 <- readLines("./inst/input11.txt") %>%
  strsplit(",") %>%
  unlist() %>%
  find_longest_hexagon_distance()

stopifnot(results2 == aoc17_solutions$day11b)
