library(adventofcode17)
lengths <- c(227, 169, 3, 166, 246, 201, 0, 47, 1, 255, 2, 254, 96, 3, 97, 144)

args <- list(
 items = 0:255,
 lengths = lengths,
 current_position = 1,
 step = 0
)

results <- twist_knot(args)
p1 <- prod(results$items[1:2])
stopifnot(p1 == aoc17_solutions$day10a)

p2 <- knot_hash("227,169,3,166,246,201,0,47,1,255,2,254,96,3,97,144")
stopifnot(p2 == aoc17_solutions$day10b)
