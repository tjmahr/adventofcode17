library(adventofcode17)
results <- readLines("./inst/input11.txt") %>%
  strsplit(",") %>%
  unlist() %>%
  hexagon_distance()

results$n_x_steps + results$n_y_steps

stopifnot(results$n_x_steps + results$n_y_steps == 796)

results2 <- readLines("./inst/input11.txt") %>%
  strsplit(",") %>%
  unlist() %>%
  find_longest_hexagon_distance()

stopifnot(results2 == 1585)
