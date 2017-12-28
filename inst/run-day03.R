library(adventofcode17)
p1 <- spiral_distance(289326)
p2 <- find_first_spiral_step_bigger_than_target(289326)

stopifnot(p1 == aoc17_solutions$day03a)
stopifnot(p2 == aoc17_solutions$day03b)
