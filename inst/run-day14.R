library(adventofcode17)
bits <- generate_grid_hashes("ljoxqyyw")

p1 <- str_sum_ones(bits)
p2 <- count_grid_regions(bits)

stopifnot(p1 == aoc17_solutions$day14a)
stopifnot(p2 == aoc17_solutions$day14b)
